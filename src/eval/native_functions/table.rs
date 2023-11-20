use std::rc::Rc;

use crate::eval::Vm;

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResult, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn insert(mut table: Table, val: RuaVal) {
    table.push(val);
}

#[rua_func]
fn remove(mut table: Table) -> Option<RuaVal> {
    table.pop()
}

pub(super) fn table(vm: &mut Vm) -> Table {
    let table = [
        ("insert", RuaVal::NativeFunction(NativeFunction::new(&insert).into())),
        ("remove", RuaVal::NativeFunction(NativeFunction::new(&remove).into())),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(table)
}
