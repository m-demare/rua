use std::{
    cell::{Cell, RefCell},
    num::NonZeroU32,
    rc::Rc,
};

use crate::{
    compiler::upvalues::UpvalueHandle,
    eval::{macros::trace_gc, GcData, Vm},
};

use super::{function::Function, IntoRuaVal, RuaVal, RuaValInner, Upvalue, UpvalueObj};

#[derive(Clone, Debug)]
pub struct Closure {
    function: Function,
    upvalues: RefCell<Vec<UpvalueObj>>,
    marked: Cell<bool>,
    vm_id: Cell<Option<NonZeroU32>>,
}

impl PartialEq for Closure {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Closure {}

impl std::hash::Hash for Closure {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::addr_of!(*self).hash(state);
    }
}

impl Closure {
    pub(crate) fn new(function: Function) -> Self {
        let upvalue_count = function.upvalue_count() as usize;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count).into(),
            marked: false.into(),
            vm_id: None.into(),
        }
    }

    #[inline]
    pub(crate) const fn function(&self) -> &Function {
        &self.function
    }

    #[allow(clippy::needless_pass_by_ref_mut)]
    pub(in crate::eval) fn push_upvalue(&mut self, upval: UpvalueObj) {
        self.upvalues.borrow_mut().push(upval);
    }

    pub(in crate::eval) fn get_upvalue(&self, up: UpvalueHandle) -> UpvalueObj {
        self.upvalues.borrow()[up.pos()].clone()
    }

    pub(in crate::eval) fn get_upvalue_val(&self, vm: &Vm, up: UpvalueHandle) -> RuaVal {
        let upvalues = self.upvalues.borrow();
        let val = match &*upvalues[up.pos()].borrow() {
            Upvalue::Open(idx) => vm.stack_at_abs(*idx).clone(),
            Upvalue::Closed(v) => v.clone(),
        };
        val
    }

    pub(in crate::eval) fn set_upvalue(&self, vm: &mut Vm, up: UpvalueHandle, val: RuaVal) {
        let upvalues = self.upvalues.borrow();
        {
            match &*upvalues[up.pos()].borrow() {
                Upvalue::Open(idx) => {
                    vm.set_stack_at_abs(*idx, val);
                    return;
                }
                Upvalue::Closed(_) => {
                    // cannot replace here since refcell is borrowed
                }
            }
        }
        upvalues[up.pos()].replace(Upvalue::Closed(val));
    }

    #[must_use]
    pub fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    #[must_use]
    pub(in crate::eval) fn mark(&self) -> bool {
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
            if let Upvalue::Closed(v) = &*up.borrow() {
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

    pub(super) fn register_in(&self, vm_id: NonZeroU32) -> bool {
        let old_id = self.vm_id.replace(Some(vm_id));
        if let Some(old_id) = old_id {
            assert!(vm_id == old_id, "Cannot register closure in a different Vm");
            false
        } else {
            true
        }
    }
}

impl IntoRuaVal for Closure {
    #[inline]
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        Rc::new(self).into_rua(vm)
    }
}

impl IntoRuaVal for Rc<Closure> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        if self.register_in(vm.id()) {
            vm.register_closure(&self);
        }
        RuaVal(RuaValInner::Closure(self))
    }
}
