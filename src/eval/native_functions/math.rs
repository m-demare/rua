use std::rc::Rc;

use crate::eval::Vm;

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResult, RuaResultUntraced, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn sqrt(n: f64) -> f64 {
    n.sqrt()
}

#[rua_func]
fn abs(n: f64) -> f64 {
    n.abs()
}

#[rua_func]
fn max(ctxt: &mut FunctionContext, _: RuaVal) -> RuaResultUntraced {
    let max = ctxt.args.iter().try_fold(f64::MIN, |acc, v| v.as_number().map(|v| acc.max(v)))?;
    Ok(max.into())
}

#[rua_func]
fn min(ctxt: &mut FunctionContext, _: RuaVal) -> RuaResultUntraced {
    let min = ctxt.args.iter().try_fold(f64::MIN, |acc, v| v.as_number().map(|v| acc.min(v)))?;
    Ok(min.into())
}

#[rua_func]
fn sin(n: f64) -> f64 {
    n.sin()
}

#[rua_func]
fn cos(n: f64) -> f64 {
    n.cos()
}

#[rua_func]
fn tan(n: f64) -> f64 {
    n.tan()
}

#[rua_func]
fn sinh(n: f64) -> f64 {
    n.sinh()
}

#[rua_func]
fn cosh(n: f64) -> f64 {
    n.cosh()
}

#[rua_func]
fn tanh(n: f64) -> f64 {
    n.tanh()
}

#[rua_func]
fn floor(n: f64) -> f64 {
    n.floor()
}

#[rua_func]
fn ceil(n: f64) -> f64 {
    n.ceil()
}

#[rua_func]
fn exp(n: f64) -> f64 {
    n.exp()
}

pub(super) fn math(vm: &mut Vm) -> Table {
    let math = [
        ("sqrt", RuaVal::NativeFunction(NativeFunction::new(&sqrt).into())),
        ("abs", RuaVal::NativeFunction(NativeFunction::new(&abs).into())),
        ("max", RuaVal::NativeFunction(NativeFunction::new(&max).into())),
        ("min", RuaVal::NativeFunction(NativeFunction::new(&min).into())),
        ("sin", RuaVal::NativeFunction(NativeFunction::new(&sin).into())),
        ("cos", RuaVal::NativeFunction(NativeFunction::new(&cos).into())),
        ("tan", RuaVal::NativeFunction(NativeFunction::new(&tan).into())),
        ("sinh", RuaVal::NativeFunction(NativeFunction::new(&sinh).into())),
        ("cosh", RuaVal::NativeFunction(NativeFunction::new(&cosh).into())),
        ("tanh", RuaVal::NativeFunction(NativeFunction::new(&tanh).into())),
        ("floor", RuaVal::NativeFunction(NativeFunction::new(&floor).into())),
        ("ceil", RuaVal::NativeFunction(NativeFunction::new(&ceil).into())),
        ("exp", RuaVal::NativeFunction(NativeFunction::new(&exp).into())),
        ("pi", std::f64::consts::PI.into()),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(math)
}
