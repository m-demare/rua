use std::rc::Rc;

use crate::eval::{
    vals::{coroutine::Coroutine, function::CoroutineAction, Callable, RuaResult},
    Vm,
};

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResultTraced, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn create(ctxt: &mut FunctionContext, func: Callable) -> RuaVal {
    Coroutine::new(func).into_rua(ctxt.vm)
}

#[rua_func]
fn resume(ctxt: &mut FunctionContext, coroutine: Rc<Coroutine>) -> RuaVal {
    if coroutine.is_dead() {
        return false.into();
    }
    ctxt.set_coroutine_action(CoroutineAction::Resume {
        coroutine,
        args_start: ctxt.args_start() + 1,
        nargs: ctxt.nargs() - 1,
    });
    ().into()
}

#[rua_func]
fn r#yield(ctxt: &mut FunctionContext, coroutine: Rc<Coroutine>) {
    ctxt.set_coroutine_action(CoroutineAction::Yield {
        coroutine,
        args_start: ctxt.args_start() + 1,
        nargs: ctxt.nargs() - 1,
    });
}

#[rua_func]
fn r#status(coroutine: Rc<Coroutine>) -> String {
    coroutine.status().to_string()
}

pub(super) fn coroutine(vm: &mut Vm) -> RuaVal {
    let coroutine = [
        ("create", NativeFunction::new(&create).into()),
        ("resume", NativeFunction::new(&resume).into()),
        ("yield", NativeFunction::new(&r#yield).into()),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(coroutine).into_rua(vm)
}
