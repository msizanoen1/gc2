#![feature(arbitrary_self_types)]

use gc2::{
    GcAccess, GcAccessible, GcBox, GcCell, GcObj, GcObjAccess, GcProject, GcProjectMut, GcRoot,
    OptionExt,
};

#[derive(GcObj, GcAccessible, GcProject, GcProjectMut)]
struct Node<T: GcObj + GcAccessible>(GcCell<NodeInner<T>>);

#[derive(GcObj, GcAccessible, GcProject, GcProjectMut)]
struct NodeInner<T: GcObj + GcAccessible> {
    prev: Option<GcBox<Node<T>>>,
    next: Option<GcBox<Node<T>>>,
    value: T,
}

impl<T: GcObj + GcAccessible> Node<T> {
    unsafe fn new_unchecked(value: T) -> GcRoot<Self> {
        let inner = NodeInner {
            prev: None,
            next: None,
            value,
        };
        let cell = Self(GcCell::new_noroot(inner));
        GcRoot::new(cell)
    }

    fn set_next(self: GcAccess<'_, Self>, next: Option<GcObjAccess<'_, Self>>) {
        self.project()
            .0
            .borrow_mut()
            .access_mut()
            .project_mut()
            .next
            .set_option(next);
    }

    fn set_prev(self: GcAccess<'_, Self>, prev: Option<GcObjAccess<'_, Self>>) {
        self.project()
            .0
            .borrow_mut()
            .access_mut()
            .project_mut()
            .prev
            .set_option(prev);
    }

    fn insert_next(self: GcObjAccess<'_, Self>, node: GcObjAccess<'_, Self>) {
        let cell = self.access().project().0;
        let next = cell
            .borrow()
            .access()
            .project()
            .next
            .get_option()
            .map(|x| x.as_root());
        let next = next.as_ref().map(|x| x.obj_access());
        node.access().set_prev(Some(self));
        node.access().set_next(next);
        if let Some(next) = next {
            next.access().set_prev(Some(node));
        }
        self.access().set_next(Some(node));
    }

    fn rm_next(self: GcObjAccess<'_, Self>) {
        let this = self.access().project().0;
        let next = if let Some(next) = this.borrow().access().project().next.get_option() {
            next.as_root()
        } else {
            return;
        };
        let next = next.obj_access().access().project().0;
        if let Some(nnext) = next.borrow().access().project().next.get_option() {
            nnext.access().set_prev(Some(self));
            self.access().set_next(Some(nnext));
        } else {
            self.access().set_next(None);
        };
    }
}

fn main() {
    let n1 = unsafe { Node::new_unchecked(1) };
    let n2 = unsafe { Node::new_unchecked(2) };
    n1.obj_access().insert_next(n2.obj_access());
    drop(n2);
    println!("1");
    gc2::collect();
    n1.obj_access().rm_next();
    println!("2");
    gc2::collect();
    println!(
        "{}",
        *n1.obj_access()
            .access()
            .project()
            .0
            .borrow()
            .access()
            .project()
            .value
    );
    drop(n1);
    println!("3");
    gc2::collect();
}
