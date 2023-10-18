#![allow(clippy::needless_pass_by_value)]
use std::convert::identity;

use crate::lex::utils::read_number_radix;

use super::vals::{TryIntoOpt, FunctionContext, LuaVal, EvalError, LuaResult};
use rua_func_macros::lua_func;

#[lua_func]
pub fn print(ctxt: &FunctionContext) {
    let s = ctxt.args.iter()
        .map(|arg| format!("{arg}"))
        .collect::<Vec<_>>()
        .join(" ");

    println!("{s}");
}

#[lua_func]
pub fn tostring(arg: LuaVal) -> String {
    arg.to_string()
}

#[lua_func]
#[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_possible_truncation)]
pub fn tonumber(s: LuaVal, radix: Option<f64>) -> LuaResult {
    let radix = radix.map_or(10.0, identity);
    let r: u32 = radix.round() as u32;
    if f64::from(r) != radix {
        return Err(EvalError::Exception(format!("Bad argument radix: {radix} is not an integer").into()))
    }
    Ok(match read_number_radix(s.to_string().chars().peekable().by_ref(), r) {
        Ok(n) => LuaVal::Number(n),
        Err(..) => LuaVal::Nil,
    })
}

#[lua_func(exact_args)]
pub fn lua_type(val: LuaVal) -> String {
    let t = val.get_type();
    t.to_string()
}

