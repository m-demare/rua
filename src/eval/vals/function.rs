#![allow(clippy::module_name_repetitions)]

use std::{
    hash::{Hash, Hasher},
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::eval::Vm;

use super::{RuaCallable, RuaResult, RuaVal};

#[derive(Clone)]
pub struct Function {
    // args: Rc<[FunctionArg]>,
    // body: Rc<[Statement]>,
    // env: Rc<RefCell<Scope>>,
    id: usize,
}

#[derive(Clone)]
pub struct NativeFunction {
    func: Rc<dyn Fn(&mut FunctionContext) -> RuaResult>,
    id: usize,
}

pub struct FunctionContext<'vm> {
    pub args: Vec<RuaVal>,
    pub vm: &'vm mut Vm,
}

impl Function {
    pub fn new(/*args: Rc<[FunctionArg]>, body: Rc<[Statement]>, env: Rc<RefCell<Scope>>*/) -> Self
    {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self { /*args, body, env,*/ id: COUNTER.fetch_add(1, Ordering::Relaxed) }
    }
}

impl NativeFunction {
    pub fn new(func: Rc<dyn Fn(&mut FunctionContext) -> RuaResult>) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self { func, id: COUNTER.fetch_add(1, Ordering::Relaxed) }
    }
}

impl RuaCallable for Function {
    fn call(&self, args: &[RuaVal], vm: &mut Vm) -> RuaResult {
        // let mut new_env = Scope::extend(self.env.clone());
        // self.args.iter().zip(args).for_each(|(arg, val)| match arg {
        //     FunctionArg::Identifier(id) => new_env.set(*id, val.clone()),
        //     FunctionArg::Dotdotdot => todo!(),
        // });

        // match eval_block(&self.body, &RefCell::new(new_env).into())? {
        //     StmtResult::None => Ok(RuaVal::Nil),
        //     StmtResult::Return(v) => Ok(v),
        //     StmtResult::Break => todo!(),
        // }
        Ok(RuaVal::Nil)
    }
}

impl RuaCallable for NativeFunction {
    fn call(&self, args: &[RuaVal], vm: &mut Vm) -> RuaResult {
        (self.func)(&mut FunctionContext::new(args.to_vec(), vm))
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Function {}

impl Eq for NativeFunction {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<'vm> FunctionContext<'vm> {
    pub fn new(args: Vec<RuaVal>, vm: &'vm mut Vm) -> Self {
        Self { args, vm }
    }
}
