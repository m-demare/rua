use super::vals::{FunctionContext, LuaVal, EvalError};

pub fn print(ctxt: FunctionContext) -> Result<LuaVal, EvalError> {
    let s = ctxt.args.iter()
        .map(|arg| format!("{arg}"))
        .collect::<Vec<_>>()
        .join(" ");

    println!("{s}");

    Ok(LuaVal::Nil)
}

