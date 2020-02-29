#![feature(arbitrary_self_types)]

pub use gc2_derive::*;

use nohash_hasher::{IntMap as HashMap, IntSet as HashSet};
use std::alloc::{self, Layout};
use std::cell::{Ref, RefCell, RefMut};
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, NonNull};
use std::thread_local;

thread_local! {
    static ALL_ROOTS: RefCell<HashMap<GcAny, usize>> = RefCell::new(HashMap::default());
    static ALL_OBJS: RefCell<HashSet<GcAny>> = RefCell::new(HashSet::default());
}

macro_rules! impl_primitive {
    ($($t:ty)*) => {
        $(
            unsafe impl GcAccessible for $t {}
            unsafe impl GcAccessibleMut for $t {}
            unsafe impl NoGc for $t {}
        )*
    };
}

macro_rules! impl_accessible_generic {
    ($(<$($g:ident),*> $t:ty),*) => {
        $(
            unsafe impl<$($g: GcAccessible),*> GcAccessible for $t {}
            unsafe impl<$($g: GcAccessibleMut),*> GcAccessibleMut for $t {}
        )*
    };
}

macro_rules! impl_accessible_generic_unsized {
    ($(<$($g:ident),*> $t:ty),*) => {
        $(
            unsafe impl<$($g: GcAccessible + ?Sized),*> GcAccessible for $t {}
            unsafe impl<$($g: GcAccessibleMut + ?Sized),*> GcAccessibleMut for $t {}
        )*
    };
}

#[allow(unused)]
macro_rules! impl_no_gc {
    ($(<$($g:ident),*> $t:ty)*) => {
        $(
            unsafe impl<$($g: NoGc),*> NoGc for $t {}
        )*
    };
}

macro_rules! impl_visit_generic_iter {
    ($(<$($g:ident),*> $t:ty),*) => {
        impl_visit_generic_iter_meth!(
            $(<$($g),*> [iter] $t),*
        );
    };
}

macro_rules! impl_visit_generic_iter_meth {
    ($(<$($g:ident),*> [$($m:ident),+] $t:ty),*) => {$(
        unsafe impl<$($g: GcObj),*> GcObj for $t {
            unsafe fn visit(&self, c: &mut $crate::Collector) {$(
                for elem in self.$m() {
                    $crate::GcObj::visit(elem, c);
                }
            )+}
        }
    )*};
}

macro_rules! impl_visit_generic_deref {
    ($(<$g:ident> $t:ty),*) => {$(
        unsafe impl<$g: GcObj + ?Sized> GcObj for $t {
            unsafe fn visit(&self, c: &mut $crate::Collector) {
                $crate::GcObj::visit(self as &$g, c);
            }
        }
    )*};
}

impl_primitive!(
    ()
    i8 i16 i32 i64 i128
    u8 u16 u32 u64 u128
    f32 f64
    char bool
    String
    std::fs::File
    std::fs::FileType
    std::fs::Metadata
    std::fs::OpenOptions
    std::io::Stdin
    std::io::Stdout
    std::io::Stderr
    std::io::Error
    std::net::TcpStream
    std::net::TcpListener
    std::net::UdpSocket
    std::net::Ipv4Addr
    std::net::Ipv6Addr
    std::net::SocketAddrV4
    std::net::SocketAddrV6
    std::path::PathBuf
    std::process::Command
    std::process::Child
    std::process::ChildStdout
    std::process::ChildStdin
    std::process::ChildStderr
    std::process::Output
    std::process::ExitStatus
    std::process::Stdio
    std::sync::Barrier
    std::sync::Condvar
    std::sync::Once
);
impl_visit_generic_deref!(
    <T> std::sync::Arc<T>,
    <T> std::rc::Rc<T>
);
impl_accessible_generic_unsized!(
    <T> std::sync::Arc<T>,
    <T> std::rc::Rc<T>
);
impl_accessible_generic!(
    <T> Vec<T>,
    <T> Option<T>,
    <T> std::collections::BTreeSet<T>,
    <T> std::collections::HashSet<T>,
    <T> std::collections::VecDeque<T>,
    <T> std::collections::LinkedList<T>,
    <T> std::collections::BinaryHeap<T>,
    <K, V> std::collections::BTreeMap<K, V>,
    <K, V> std::collections::HashMap<K, V>
);
impl_visit_generic_iter!(
    <T> Vec<T>,
    <T> Option<T>,
    <T> std::collections::BTreeSet<T>,
    <T> std::collections::HashSet<T>,
    <T> std::collections::VecDeque<T>,
    <T> std::collections::LinkedList<T>,
    <T> std::collections::BinaryHeap<T>
);
impl_visit_generic_iter_meth!(
    <K, V> [keys, values] std::collections::BTreeMap<K, V>,
    <K, V> [keys, values] std::collections::HashMap<K, V>
);

unsafe impl<T: GcObj> GcAccessibleMut for GcCell<T> {}
unsafe impl<T: GcObj> GcAccessible for GcCell<T> {}
unsafe impl<T: GcObj + ?Sized> GcAccessible for GcBox<T> {}

unsafe impl<T: GcObj> GcObj for RefCell<T> {
    unsafe fn visit(&self, c: &mut Collector) {
        self.borrow().visit(c);
    }
}

unsafe impl<T: GcObj + ?Sized> GcObj for GcBox<T> {
    unsafe fn visit(&self, c: &mut Collector) {
        let ptr = (self.0).0.as_ptr() as *const ();
        // Safety: This is incredibly unsafe, however the `Hash` and `Eq` impls just cast it
        // to thin pointers of type `*const ()` and this `ptr` is only used as keys to the
        // `IntSet`, there shouldn't be any undefined behavior.
        let ptr: *mut (dyn GcObj + 'static) = mem::transmute([ptr as usize, 0usize]);
        let ptr = GcPtr(NonNull::new_unchecked(ptr));
        if let Some(ptr) = c.whites.take(&ptr) {
            // Safety: This `ptr` is taken from the data structure above.
            c.grays.insert(ptr);
        }
    }
}

impl nohash_hasher::IsEnabled for GcAny {}

pub struct Collector {
    whites: HashSet<GcAny>,
    grays: HashSet<GcAny>,
    blacks: HashSet<GcAny>,
}

pub fn collect() {
    let grays = ALL_ROOTS.with(|x| x.borrow().keys().cloned().collect::<HashSet<_>>());
    let whites = ALL_OBJS
        .with(|x| x.borrow().clone())
        .into_iter()
        .filter(|x| !grays.contains(x))
        .collect();
    let mut c = Collector {
        whites,
        grays,
        blacks: HashSet::default(),
    };
    loop {
        let current = mem::take(&mut c.grays);
        for obj in current {
            unsafe {
                obj.as_ref().visit(&mut c);
            }
            c.blacks.insert(obj);
        }
        if c.grays.is_empty() {
            break;
        }
    }
    ALL_OBJS.with(|all| {
        let mut all = all.borrow_mut();
        for obj in c.whites {
            unsafe {
                let ptr = obj.0.as_ptr();
                let layout = Layout::for_value(obj.0.as_ref());
                if layout.size() > 0 {
                    alloc::dealloc(ptr as *mut u8, layout);
                }
            }
            all.remove(&obj);
        }
    });
}

pub unsafe trait GcObj {
    /// # Safety
    ///
    /// All references in the object must be valid.
    unsafe fn visit(&self, c: &mut Collector);
}

unsafe impl<T: NoGc> GcObj for T {
    unsafe fn visit(&self, _c: &mut Collector) {}
}

pub unsafe trait GcAccessible: GcObj {}
pub unsafe trait GcAccessibleMut: GcAccessible {}
pub unsafe trait NoGc {}

unsafe impl<T: ?Sized> NoGc for &'_ T {}
unsafe impl<T: ?Sized> NoGc for &'_ mut T {}

struct GcPtr<T: ?Sized + GcObj>(NonNull<T>);

impl<T: ?Sized + GcObj> Copy for GcPtr<T> {}

impl<T: ?Sized + GcObj> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

type GcAny = GcPtr<dyn GcObj>;

impl<T: ?Sized + GcObj> Hash for GcPtr<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        let ptr = self.0.as_ptr() as *const ();
        ptr.hash(hasher);
    }
}

impl<T: ?Sized + GcObj> PartialEq for GcPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        let ptr = self.0.as_ptr() as *const ();
        let otr = other.0.as_ptr() as *const ();
        ptr == otr
    }
}

impl<T: ?Sized + GcObj> PartialOrd for GcPtr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let ptr = self.0.as_ptr() as *const ();
        let otr = other.0.as_ptr() as *const ();
        Some(ptr.cmp(&otr))
    }
}

impl<T: ?Sized + GcObj> Ord for GcPtr<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<T: ?Sized + GcObj> Eq for GcPtr<T> {}

impl<'a, T: GcObj + 'a> GcPtr<T> {
    fn as_any_obj(self) -> GcAny {
        #[allow(clippy::transmute_ptr_to_ptr)]
        let ptr: *mut (dyn GcObj + 'static) =
            unsafe { mem::transmute(self.0.as_ptr() as *mut (dyn GcObj + 'a)) };
        GcPtr(NonNull::new(ptr).unwrap())
    }
}

impl GcAny {
    unsafe fn as_ref(&self) -> &dyn GcObj {
        &*self.0.as_ptr()
    }
}

impl<T: GcObj + ?Sized> Copy for GcBox<T> {}
impl<T: GcObj + ?Sized> Clone for GcBox<T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct GcRoot<T: GcObj>(GcPtr<T>);
pub struct GcBox<T: GcObj + ?Sized>(GcPtr<T>);
pub struct GcObjAccess<'a, T: GcObj + ?Sized>(GcPtr<T>, PhantomData<&'a T>);
pub struct GcAccess<'a, T: GcObj + ?Sized>(NonNull<T>, PhantomData<&'a T>);
pub struct GcObjAccessMut<'a, T: GcObj + ?Sized>(GcPtr<T>, PhantomData<&'a mut T>);
pub struct GcAccessMut<'a, T: GcObj + ?Sized>(NonNull<T>, PhantomData<&'a mut T>);
impl<'a, T: GcObj + ?Sized> Copy for GcAccess<'a, T> {}
impl<'a, T: GcObj + ?Sized> Clone for GcAccess<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: GcObj + ?Sized> GcAccessMut<'a, T> {
    pub fn reborrow(&mut self) -> GcAccessMut<T> {
        GcAccessMut(self.0, PhantomData)
    }

    pub fn access(&self) -> GcAccess<T> {
        GcAccess(self.0, PhantomData)
    }

    /// # Safety
    ///
    /// raw must be valid and non-null.
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        GcAccessMut(NonNull::new_unchecked(raw), PhantomData)
    }

    /// # Safety
    ///
    /// The user must not insert a dangling GcBox through the reference.
    pub unsafe fn get_mut_unchecked(&mut self) -> &mut T {
        self.0.as_mut()
    }
}

impl<'a, T: GcObj + GcAccessible + ?Sized> Deref for GcAccessMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<'a, T: GcObj + GcAccessibleMut + ?Sized> DerefMut for GcAccessMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<'a, T: GcObj + ?Sized> GcAccess<'a, T> {
    /// # Safety
    ///
    /// `ptr` must point to a GC object.
    pub unsafe fn from_raw(ptr: *const T) -> Self {
        GcAccess(NonNull::new(ptr as *mut T).unwrap(), PhantomData)
    }

    /// # Safety
    ///
    /// You must not insert dangling GcBox objects through the reference.
    pub unsafe fn get_unchecked(self) -> &'a T {
        &*self.0.as_ptr()
    }
}

impl<'a, T: GcObj + GcAccessible + ?Sized> GcAccess<'a, T> {
    pub fn get_ref(self) -> &'a T {
        unsafe { self.get_unchecked() }
    }
}

impl<'a, T: GcObj + ?Sized> Copy for GcObjAccess<'a, T> {}
impl<'a, T: GcObj + ?Sized> Clone for GcObjAccess<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: GcObj> GcObjAccess<'a, T> {
    pub fn access(self) -> GcAccess<'a, T> {
        GcAccess((self.0).0, PhantomData)
    }

    pub fn as_gc_box(self) -> GcBox<T> {
        GcBox(self.0)
    }

    pub fn as_root(self) -> GcRoot<T> {
        unsafe { GcRoot::from_gc_box(self.as_gc_box()) }
    }
}

impl<'a, T: GcObj + GcAccessible + ?Sized> Deref for GcObjAccess<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { (self.0).0.as_ref() }
    }
}

impl<T: GcObj + ?Sized> GcBox<T> {
    /// # Safety
    ///
    /// The object and all references within must be valid.
    pub unsafe fn obj_access_unchecked<'a>(self) -> GcObjAccess<'a, T> {
        GcObjAccess(self.0, PhantomData)
    }

    #[allow(clippy::needless_lifetimes)]
    pub fn obj_access<'a>(self: GcAccess<'a, Self>) -> GcObjAccess<'a, T> {
        unsafe { self.obj_access_unchecked() }
    }
}

impl<T: GcObj> GcAccessMut<'_, GcBox<T>> {
    pub fn set(&mut self, val: GcObjAccess<'_, T>) {
        unsafe {
            *self.get_mut_unchecked() = val.as_gc_box();
        }
    }
}

pub struct GcCell<T: GcObj> {
    inner: RefCell<T>,
}

unsafe impl<T: GcObj> GcObj for GcCell<T> {
    unsafe fn visit(&self, c: &mut Collector) {
        self.inner.visit(c);
    }
}

impl<T: GcObj> GcCell<T> {
    /// # Safety
    ///
    /// All references in `val` must be valid
    pub unsafe fn new(val: T) -> GcRoot<Self> {
        GcRoot::new(Self::new_noroot(val))
    }

    pub fn new_noroot(val: T) -> Self {
        GcCell {
            inner: RefCell::new(val),
        }
    }

    pub fn borrow<'a>(self: &'a GcAccess<Self>) -> GcCellRef<'a, T> {
        let val = unsafe { self.0.as_ref().inner.borrow() };
        GcCellRef { val }
    }

    pub fn borrow_mut<'a>(self: &'a GcAccess<Self>) -> GcCellRefMut<'a, T> {
        let val = unsafe { self.0.as_ref().inner.borrow_mut() };
        GcCellRefMut { val }
    }
}

pub struct GcCellRef<'a, T: GcObj> {
    val: Ref<'a, T>,
}

impl<'a, T: GcObj> GcCellRef<'a, T> {
    pub fn access(&self) -> GcAccess<T> {
        unsafe { GcAccess::from_raw(&*self.val) }
    }
}

impl<'a, T: GcObj + GcAccessible> Deref for GcCellRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.val
    }
}

pub struct GcCellRefMut<'a, T: GcObj> {
    val: RefMut<'a, T>,
}

impl<'a, T: GcObj> GcCellRefMut<'a, T> {
    pub fn access(&self) -> GcAccess<T> {
        unsafe { GcAccess::from_raw(&*self.val) }
    }

    pub fn access_mut(&mut self) -> GcAccessMut<T> {
        unsafe { GcAccessMut::from_raw(&mut *self.val) }
    }
}

impl<'a, T: GcObj + GcAccessible + ?Sized> Deref for GcAccess<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.get_ref()
    }
}

impl<T: GcObj> GcRoot<T> {
    /// # Safety
    ///
    /// All references in `val` must be valid
    pub unsafe fn new(val: T) -> Self {
        let layout = Layout::new::<T>();
        let ptr = if layout.size() > 0 {
            let ptr = alloc::alloc(layout) as *mut T;
            assert!(!ptr.is_null());
            ptr::write(ptr, val);
            NonNull::new_unchecked(ptr)
        } else {
            NonNull::dangling()
        };
        let ptr = GcPtr(ptr);
        ALL_OBJS.with(|objs| {
            let mut objs = objs.borrow_mut();
            objs.insert(ptr.as_any_obj());
        });
        Self::from_gc_box(GcBox(ptr))
    }

    /// # Safety
    ///
    /// The object and all references within must be valid.
    pub unsafe fn from_gc_box(gc: GcBox<T>) -> Self {
        let ptr = gc.0;
        let ptri = ptr.as_any_obj();
        ALL_ROOTS.with(|objs| {
            *objs.borrow_mut().entry(ptri).or_insert(0) += 1;
        });
        Self(ptr)
    }

    pub fn as_gc_box(&self) -> GcBox<T> {
        GcBox(self.0)
    }

    pub fn obj_access(&self) -> GcObjAccess<T> {
        GcObjAccess(self.0, PhantomData)
    }

    pub fn access(&self) -> GcAccess<T> {
        GcAccess((self.0).0, PhantomData)
    }
}

impl<T: GcObj + ?Sized> GcBox<T> {
    /// # Safety
    ///
    /// The object must be valid.
    pub unsafe fn get_unchecked(&self) -> &T {
        (self.0).0.as_ref()
    }
}

impl<T: GcObj> Drop for GcRoot<T> {
    fn drop(&mut self) {
        ALL_ROOTS.with(|roots| {
            let mut roots = roots.borrow_mut();
            let count = roots.get_mut(&self.0.as_any_obj()).unwrap();
            *count -= 1;
            if *count == 0 {
                roots.remove(&self.0.as_any_obj());
            }
        });
    }
}

impl<T: GcObj> Clone for GcRoot<T> {
    fn clone(&self) -> Self {
        unsafe { GcRoot::from_gc_box(self.as_gc_box()) }
    }
}

pub trait OptionExt<T: GcObj + ?Sized>: GcAccessible {
    #[allow(clippy::needless_lifetimes)]
    fn get_option<'a>(self: GcAccess<'a, Self>) -> Option<GcObjAccess<'a, T>>;
    fn set_option(self: &mut GcAccessMut<Self>, other: Option<GcObjAccess<T>>);
}

impl<T: GcObj> OptionExt<T> for Option<GcBox<T>> {
    #[allow(clippy::needless_lifetimes)]
    fn get_option<'a>(self: GcAccess<'a, Self>) -> Option<GcObjAccess<'a, T>> {
        self.map(|x| unsafe { x.obj_access_unchecked() })
    }
    fn set_option(self: &mut GcAccessMut<Self>, other: Option<GcObjAccess<T>>) {
        unsafe {
            *self.get_mut_unchecked() = other.map(|x| x.as_gc_box());
        }
    }
}
