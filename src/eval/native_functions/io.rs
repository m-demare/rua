use std::rc::Rc;

use crate::eval::Vm;

use super::super::vals::{
    function::{FunctionContext, NativeFunction},
    table::Table,
    IntoRuaVal, RuaResultTraced, RuaVal,
};
use rua_func_macros::rua_func;

#[rua_func]
fn io_write(ctxt: &FunctionContext) {
    use std::fmt::Write;

    let s = ctxt.args().iter().fold(String::new(), |mut out, arg| {
        let _ = write!(out, "{arg}");
        out
    });

    print!("{s}");
}

pub(super) fn io(vm: &mut Vm) -> RuaVal {
    let io = [("write", NativeFunction::new(&io_write).into())]
        .map(|(k, v)| (Into::<Rc<str>>::into(k).into_rua(vm), v));

    Table::from_iter(io).into_rua(vm)
}
