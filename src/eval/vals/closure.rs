use std::cell::RefCell;

use either::Either::{self, Left, Right};

use crate::{compiler::upvalues::UpvalueHandle, eval::Vm};

use super::{function::Function, RuaVal, UpvalueObj};

#[derive(Clone, Eq)]
pub struct Closure {
    function: Function,
    upvalues: Vec<UpvalueObj>,
    marked: RefCell<bool>,
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
        Self { function, upvalues: Vec::with_capacity(upvalue_count), marked: false.into() }
    }

    pub const fn function(&self) -> &Function {
        &self.function
    }

    pub fn push_upvalue(&mut self, upval: UpvalueObj) {
        self.upvalues.push(upval);
    }

    pub fn get_upvalue(&self, up: UpvalueHandle) -> UpvalueObj {
        self.upvalues[up.get()].clone()
    }

    pub fn get_upvalue_val(&self, vm: &Vm, up: UpvalueHandle) -> RuaVal {
        let location: &Either<usize, RuaVal> = &self.upvalues[up.get()].borrow();
        match location {
            Left(idx) => vm.stack_at(*idx),
            Right(v) => v.clone(),
        }
    }

    pub fn set_upvalue(&self, vm: &mut Vm, up: UpvalueHandle, val: RuaVal) {
        {
            let location: &Either<usize, RuaVal> = &self.upvalues[up.get()].borrow();
            match location {
                Left(idx) => {
                    vm.set_stack_at(*idx, val);
                    return;
                }
                Right(_) => {
                    // cannot replace here since refcell is borrowed
                }
            }
        }
        self.upvalues[up.get()].replace(Right(val));
    }

    pub(crate) fn mark(&self) {
        {
            let already_marked = self.marked.borrow();
            if *already_marked {
                return;
            }
        }

        #[cfg(test)]
        println!("Marked {}", std::ptr::addr_of!(*self) as usize);

        self.marked.replace(true);

        for up in &self.upvalues {
            if let Some(v) = up.borrow().as_ref().right() {
                v.mark();
            }
        }
    }
}
