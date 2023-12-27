use std::{
    cell::{Cell, RefCell},
    num::NonZeroU32,
    rc::Rc,
};

use either::Either::{self, Left, Right};

use crate::{
    compiler::upvalues::UpvalueHandle,
    eval::{macros::trace_gc, GcData, Vm},
};

use super::{function::Function, IntoRuaVal, RuaVal, RuaValInner, UpvalueObj};

#[derive(Clone, Eq, Debug)]
pub struct Closure {
    function: Function,
    upvalues: RefCell<Vec<UpvalueObj>>,
    marked: Cell<bool>,
    vm_id: Cell<Option<NonZeroU32>>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl std::hash::Hash for Closure {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::addr_of!(*self).hash(state);
    }
}

impl Closure {
    pub fn new(function: Function) -> Self {
        let upvalue_count = function.upvalue_count() as usize;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count).into(),
            marked: false.into(),
            vm_id: None.into(),
        }
    }

    pub const fn function(&self) -> &Function {
        &self.function
    }

    pub fn push_upvalue(&mut self, upval: UpvalueObj) {
        self.upvalues.borrow_mut().push(upval);
    }

    pub fn get_upvalue(&self, up: UpvalueHandle) -> UpvalueObj {
        self.upvalues.borrow()[up.get()].clone()
    }

    pub fn get_upvalue_val(&self, vm: &Vm, up: UpvalueHandle) -> RuaVal {
        let upvalues = self.upvalues.borrow();
        let location: &Either<_, _> = &upvalues[up.get()].borrow();
        match location {
            Left(idx) => vm.stack_at_abs(*idx).clone(),
            Right(v) => v.clone(),
        }
    }

    pub fn set_upvalue(&self, vm: &mut Vm, up: UpvalueHandle, val: RuaVal) {
        let upvalues = self.upvalues.borrow();
        {
            let location: &Either<_, _> = &upvalues[up.get()].borrow();
            match location {
                Left(idx) => {
                    vm.set_stack_at_abs(*idx, val);
                    return;
                }
                Right(_) => {
                    // cannot replace here since refcell is borrowed
                }
            }
        }
        upvalues[up.get()].replace(Right(val));
    }

    pub fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    #[must_use]
    pub(in super::super) fn mark(&self) -> bool {
        let already_marked = self.marked.replace(true);
        if already_marked {
            return false;
        }

        trace_gc!("Marked closure 0x{:x}", self.addr());

        true
    }

    pub(super) fn blacken(&self, gc_data: &mut GcData) {
        let upvalues: &Vec<_> = &self.upvalues.borrow();
        for up in upvalues {
            if let Right(v) = up.borrow().as_ref() {
                v.mark(gc_data);
            }
        }
        // Note: Chunk's constants don't need to be marked, since they can only
        // be f64, RuaString, or Function (not Closure), none of which need GC
    }

    pub(in super::super) fn soft_drop(&self) {
        trace_gc!("soft dropping closure 0x{:x}", self.addr());
        self.upvalues.borrow_mut().clear();
    }

    pub(in super::super) fn unmark(&self) -> bool {
        self.marked.replace(false)
    }

    #[must_use]
    fn register_in(&self, vm: &Vm) -> bool {
        let id = vm.id();
        let old_id = self.vm_id.replace(Some(id));
        if let Some(old_id) = old_id {
            assert!(id == old_id, "Cannot register closure in a different Vm");
            false
        } else {
            true
        }
    }
}

impl IntoRuaVal for Closure {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        Rc::new(self).into_rua(vm)
    }
}

impl IntoRuaVal for Rc<Closure> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        if self.register_in(vm) {
            vm.register_closure(&self);
        }
        RuaVal(RuaValInner::Closure(self))
    }
}
