use std::rc::Rc;

use crate::eval::Vm;

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResultTraced, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn insert(table: Rc<Table>, val: RuaVal) {
    table.push(val);
}

#[rua_func]
fn remove(table: Rc<Table>) -> Option<RuaVal> {
    table.pop()
}

pub(super) fn table(vm: &mut Vm) -> RuaVal {
    let table = [
        ("insert", NativeFunction::new(&insert).into()),
        ("remove", NativeFunction::new(&remove).into()),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(table).into_rua(vm)
}
