#![allow(clippy::needless_pass_by_value)]
use std::{convert::identity, rc::Rc};

use crate::{eval::Vm, lex::utils::read_number_radix};

use super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    EvalError, EvalErrorTraced, IntoRuaVal, RuaResult, RuaResultUntraced, RuaVal, TryIntoOpt,
};
use either::Either::{Left, Right};
use rua_func_macros::rua_func;

mod io;
mod math;
mod table;

// Built in functions {{{

#[rua_func]
fn print(ctxt: &FunctionContext) {
    let s = ctxt.args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(" ");

    println!("{s}");
}

#[rua_func]
fn tostring(arg: RuaVal) -> String {
    arg.to_string()
}

#[rua_func]
#[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_possible_truncation)]
fn tonumber(s: RuaVal, radix: Option<f64>) -> RuaResultUntraced {
    let radix = radix.map_or(10.0, identity);
    let r: u32 = radix.round() as u32;
    if f64::from(r) != radix {
        return Err(EvalError::Exception(
            format!("Bad argument radix: {radix} is not an integer").into(),
        ));
    }
    Ok(
        match read_number_radix(
            s.to_string().bytes().peekable().by_ref(),
            r.try_into().or(Err(EvalError::Exception("Invalid radix".into())))?,
        ) {
            Ok(n) => n.into(),
            Err(..) => RuaVal::nil(),
        },
    )
}

#[rua_func(exact_args)]
fn rua_type(val: RuaVal) -> String {
    let t = val.get_type();
    t.to_string()
}

#[rua_func]
fn assert(assertion: RuaVal, err: Option<RuaVal>) -> RuaResultUntraced {
    if assertion.truthy() {
        Ok(assertion)
    } else {
        Err(EvalError::AssertionFailed(err))
    }
}

// TODO revise when I add returning multiple values
#[rua_func]
fn pcall(ctxt: &mut FunctionContext, func: RuaVal) -> RuaVal {
    match func.into_callable() {
        Ok(Left(closure)) => ctxt.vm.interpret(closure),
        Ok(Right(native_fn)) => native_fn.call(&ctxt.args[1..], ctxt.vm),
        Err(_) => return false.into(),
    }
    .map_or(false.into(), |v| v)
}

#[rua_func]
fn collectgarbage(ctxt: &mut FunctionContext) {
    ctxt.vm.gc();
}

// }}}

// Helpers {{{

pub fn default_global(vm: &mut Vm) -> Rc<Table> {
    let global = [
        ("print", (NativeFunction::new(&print).into())),
        ("tostring", (NativeFunction::new(&tostring).into())),
        ("tonumber", (NativeFunction::new(&tonumber).into())),
        ("type", (NativeFunction::new(&rua_type).into())),
        ("assert", (NativeFunction::new(&assert).into())),
        ("pcall", (NativeFunction::new(&pcall).into())),
        ("collectgarbage", (NativeFunction::new(&collectgarbage).into())),
        ("table", table::table(vm)),
        ("math", math::math(vm)),
        ("io", io::io(vm)),
    ]
    .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    let global = Rc::new(Table::from_iter(global));
    // SAFETY: global doesn't need to be garbage collected, since it'll be valid
    // as long as the Vm is valid, and it's dropped when the Vm is dropped
    global.insert(
        Into::<Rc<str>>::into("_G").into_rua(vm),
        RuaVal::from_table_unregistered(global.clone()),
    );

    global
}

// }}}
