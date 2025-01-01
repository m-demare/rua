use std::rc::Rc;

use crate::eval::{
    vals::{table::try_into_usize, RuaType},
    Vm,
};
use rand::{rngs::StdRng, Rng, SeedableRng};

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResult, RuaResultTraced, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn sqrt(n: f64) -> f64 {
    n.sqrt()
}

#[rua_func]
const fn abs(n: f64) -> f64 {
    n.abs()
}

#[rua_func]
fn modf(_: f64) -> f64 {
    todo!()
}

#[rua_func]
fn fmod(m: f64, n: f64) -> f64 {
    m % n
}

#[rua_func]
fn max(ctxt: &mut FunctionContext, _: RuaVal) -> RuaResult {
    let max = ctxt.args().iter().try_fold(f64::MIN, |acc, v| v.as_number().map(|v| acc.max(v)))?;
    Ok(max.into())
}

#[rua_func]
fn min(ctxt: &mut FunctionContext, _: RuaVal) -> RuaResult {
    let min = ctxt.args().iter().try_fold(f64::MIN, |acc, v| v.as_number().map(|v| acc.min(v)))?;
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
fn asin(n: f64) -> f64 {
    n.asin()
}

#[rua_func]
fn acos(n: f64) -> f64 {
    n.acos()
}

#[rua_func]
fn atan(n: f64) -> f64 {
    n.atan()
}

#[rua_func]
fn atan2(n: f64, m: f64) -> f64 {
    n.atan2(m)
}

#[rua_func]
fn floor(n: f64) -> f64 {
    n.floor()
}

#[rua_func]
fn ceil(n: f64) -> f64 {
    n.ceil()
}

// Non-standard
#[rua_func]
fn round(n: f64) -> f64 {
    n.round()
}

#[rua_func]
fn exp(n: f64) -> f64 {
    n.exp()
}

#[rua_func]
fn pow(m: f64, n: f64) -> f64 {
    m.powf(n)
}

#[rua_func]
fn ldexp(m: f64, n: f64) -> f64 {
    m * n.exp2()
}

#[rua_func]
fn frexp(_: f64) -> f64 {
    todo!()
}

// Non-standard (lua assumes base e)
#[rua_func]
fn log(n: f64, base: Option<f64>) -> f64 {
    base.map_or_else(|| n.ln(), |base| n.log(base))
}

#[rua_func]
fn log10(n: f64) -> f64 {
    n.log10()
}

#[rua_func]
const fn deg(n: f64) -> f64 {
    n.to_degrees()
}

#[rua_func]
const fn rad(n: f64) -> f64 {
    n.to_radians()
}

#[rua_func(exact_args)]
#[allow(clippy::cast_precision_loss)]
fn random(ctxt: &mut FunctionContext, n: Option<f64>, m: Option<f64>) -> RuaResult {
    let rng = &mut ctxt.vm.rng;
    match (n.map(f64::floor).map(try_into_usize), m.map(f64::floor).map(try_into_usize)) {
        (Some(Some(n)), Some(Some(m))) if n <= m => Ok((rng.gen_range(n..=m) as f64).into()),
        (Some(Some(n)), None) => Ok((rng.gen_range(1..=n) as f64).into()),
        (None, None) => Ok(rng.gen::<f64>().into()),
        (None, Some(_)) => {
            Err(EvalError::TypeError { expected: RuaType::Number, got: RuaType::Nil })
        }
        _ => Err(EvalError::Exception("Interval is empty".into())),
    }
}

#[rua_func(exact_args)]
#[allow(clippy::cast_precision_loss)]
fn randomseed(ctxt: &mut FunctionContext, mut n: f64) {
    if n.is_nan() || n.is_subnormal() {
        n = 0.0;
    }
    ctxt.vm.rng = StdRng::seed_from_u64(n.to_bits());
}

pub(super) fn math(vm: &mut Vm) -> RuaVal {
    let math = [
        ("sqrt", NativeFunction::new(&sqrt).into()),
        ("abs", NativeFunction::new(&abs).into()),
        ("modf", NativeFunction::new(&modf).into()),
        ("fmod", NativeFunction::new(&fmod).into()),
        ("max", NativeFunction::new(&max).into()),
        ("min", NativeFunction::new(&min).into()),
        ("sin", NativeFunction::new(&sin).into()),
        ("cos", NativeFunction::new(&cos).into()),
        ("tan", NativeFunction::new(&tan).into()),
        ("sinh", NativeFunction::new(&sinh).into()),
        ("cosh", NativeFunction::new(&cosh).into()),
        ("tanh", NativeFunction::new(&tanh).into()),
        ("asin", NativeFunction::new(&asin).into()),
        ("acos", NativeFunction::new(&acos).into()),
        ("atan", NativeFunction::new(&atan).into()),
        ("atan2", NativeFunction::new(&atan2).into()),
        ("floor", NativeFunction::new(&floor).into()),
        ("ceil", NativeFunction::new(&ceil).into()),
        ("round", NativeFunction::new(&round).into()),
        ("exp", NativeFunction::new(&exp).into()),
        ("ldexp", NativeFunction::new(&ldexp).into()),
        ("frexp", NativeFunction::new(&frexp).into()),
        ("log", NativeFunction::new(&log).into()),
        ("deg", NativeFunction::new(&deg).into()),
        ("rad", NativeFunction::new(&rad).into()),
        ("random", NativeFunction::new(&random).into()),
        ("randomseed", NativeFunction::new(&randomseed).into()),
        ("pi", std::f64::consts::PI.into()),
        ("huge", f64::INFINITY.into()),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(math).into_rua(vm)
}
