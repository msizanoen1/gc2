#![feature(arbitrary_self_types)]

use gc2::{
    GcAccess, GcAccessible, GcBox, GcCell, GcObj, GcObjAccess, GcProject, GcProjectMut, GcRoot,
    OptionExt,
};

#[derive(GcObj, GcAccessible, GcProject, GcProjectMut)]
struct Node<T: GcObj + GcAccessible> {
    prev: GcCell<Option<GcBox<Node<T>>>>,
    next: GcCell<Option<GcBox<Node<T>>>>,
    value: T,
}

impl<T: GcObj + GcAccessible> Node<T> {
    unsafe fn new(value: T) -> GcRoot<Self> {
        GcRoot::new(Self {
            prev: GcCell::new_noroot(None),
            next: GcCell::new_noroot(None),
            value,
        })
    }

    #[allow(dead_code)]
    fn prev(self: GcAccess<Self>) -> Option<GcRoot<Self>> {
        self.project()
            .prev
            .borrow()
            .access()
            .get_option()
            .map(|x| x.as_root())
    }

    fn next(self: GcAccess<Self>) -> Option<GcRoot<Self>> {
        self.project()
            .prev
            .borrow()
            .access()
            .get_option()
            .map(|x| x.as_root())
    }

    fn set_next(self: GcAccess<Self>, other: Option<GcObjAccess<Self>>) {
        self.project()
            .next
            .borrow_mut()
            .access_mut()
            .set_option(other);
    }

    fn set_prev(self: GcAccess<Self>, other: Option<GcObjAccess<Self>>) {
        self.project()
            .prev
            .borrow_mut()
            .access_mut()
            .set_option(other);
    }

    fn insert_next(self: GcObjAccess<Self>, other: GcObjAccess<Self>) {
        let next = self.access().next();
        let next = next.as_ref().map(|x| x.obj_access());
        other.access().set_next(next);
        other.access().set_prev(Some(self));
        self.access().set_next(Some(other));
        if let Some(next) = next {
            next.access().set_prev(Some(other));
        }
    }

    fn remove_next(self: GcObjAccess<Self>) {
        let next = match self.access().next() {
            Some(next) => next,
            None => return,
        };
        let nnext = next.access().next();
        let nnext = nnext.as_ref().map(|x| x.obj_access());
        self.access().set_next(nnext);
        if let Some(nnext) = nnext {
            nnext.access().set_prev(Some(self));
        }
    }
}

fn main() {
    let n1 = unsafe { Node::new(1) };
    let n2 = unsafe { Node::new(2) };
    n1.obj_access().insert_next(n2.obj_access());
    drop(n2);
    gc2::collect();
    n1.obj_access().remove_next();
    gc2::collect();
    drop(n1);
    gc2::collect();
}
