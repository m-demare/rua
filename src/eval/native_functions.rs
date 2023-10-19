#![allow(clippy::needless_pass_by_value)]
use std::convert::identity;

use crate::lex::utils::read_number_radix;

use super::vals::{EvalError, FunctionContext, RuaResult, RuaVal, TryIntoOpt};
use rua_func_macros::rua_func;

#[rua_func]
pub fn print(ctxt: &FunctionContext) {
    let s = ctxt.args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(" ");

    println!("{s}");
}

#[rua_func]
pub fn tostring(arg: RuaVal) -> String {
    arg.to_string()
}

#[rua_func]
#[allow(clippy::float_cmp, clippy::cast_sign_loss, clippy::cast_possible_truncation)]
pub fn tonumber(s: RuaVal, radix: Option<f64>) -> RuaResult {
    let radix = radix.map_or(10.0, identity);
    let r: u32 = radix.round() as u32;
    if f64::from(r) != radix {
        return Err(EvalError::Exception(
            format!("Bad argument radix: {radix} is not an integer").into(),
        ));
    }
    Ok(match read_number_radix(s.to_string().chars().peekable().by_ref(), r) {
        Ok(n) => RuaVal::Number(n),
        Err(..) => RuaVal::Nil,
    })
}

#[rua_func(exact_args)]
pub fn rua_type(val: RuaVal) -> String {
    let t = val.get_type();
    t.to_string()
}
