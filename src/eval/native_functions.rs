#![allow(clippy::needless_pass_by_value)]
use std::{convert::identity, rc::Rc};

use crate::{eval::Vm, lex::utils::read_number_radix};

use super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResult, RuaResultUntraced, RuaVal, TryIntoOpt,
};
use rua_func_macros::rua_func;

// Built in functions {{{

#[rua_func]
pub fn print(ctxt: &mut FunctionContext) {
    let s = ctxt.args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(" ");

    println!("{s}");
}

#[rua_func]
pub fn tostring(arg: RuaVal) -> String {
    arg.to_string()
}

#[rua_func]
#[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_possible_truncation)]
pub fn tonumber(s: RuaVal, radix: Option<f64>) -> RuaResultUntraced {
    let radix = radix.map_or(10.0, identity);
    let r: u32 = radix.round() as u32;
    if f64::from(r) != radix {
        return Err(EvalError::Exception(
            format!("Bad argument radix: {radix} is not an integer").into(),
        ));
    }
    Ok(match read_number_radix(s.to_string().chars().peekable().by_ref(), r) {
        Ok(n) => RuaVal::Number(n.into()),
        Err(..) => RuaVal::Nil,
    })
}

#[rua_func(exact_args)]
pub fn rua_type(val: RuaVal) -> String {
    let t = val.get_type();
    t.to_string()
}

#[rua_func]
pub fn assert(assertion: RuaVal, err: Option<RuaVal>) -> RuaResultUntraced {
    if assertion.truthy() {
        Ok(assertion)
    } else {
        Err(EvalError::AssertionFailed(err))
    }
}

// TODO revise when I add returning multiple values
#[rua_func]
pub fn pcall(ctxt: &mut FunctionContext, func: RuaVal) -> RuaVal {
    match func {
        RuaVal::Closure(c) => ctxt.vm.interpret(c),
        RuaVal::NativeFunction(f) => f.call(&ctxt.args[1..], ctxt.vm),
        _ => return RuaVal::Bool(false),
    }
    .map_or(RuaVal::Bool(false), |v| v)
}

#[rua_func]
pub fn table_insert(mut table: Table, val: RuaVal) {
    table.push(val);
}

#[rua_func]
pub fn table_remove(mut table: Table) -> Option<RuaVal> {
    table.pop()
}

// }}}

// Helpers {{{

pub fn default_global(vm: &mut Vm) -> Table {
    let table = [
        ("insert", RuaVal::NativeFunction(NativeFunction::new(&table_insert).into())),
        ("remove", RuaVal::NativeFunction(NativeFunction::new(&table_remove).into())),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));
    let global = [
        ("print", RuaVal::NativeFunction(NativeFunction::new(&print).into())),
        ("tostring", RuaVal::NativeFunction(NativeFunction::new(&tostring).into())),
        ("tonumber", RuaVal::NativeFunction(NativeFunction::new(&tonumber).into())),
        ("type", RuaVal::NativeFunction(NativeFunction::new(&rua_type).into())),
        ("assert", RuaVal::NativeFunction(NativeFunction::new(&assert).into())),
        ("pcall", RuaVal::NativeFunction(NativeFunction::new(&pcall).into())),
        ("table", RuaVal::Table(Table::from_iter(table))),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(global)
}

// }}}
