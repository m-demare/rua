use std::{cell::RefCell, rc::Rc};

use crate::parser::ast::Program;

use self::{
    isolate::Isolate,
    scope::Scope,
    statements::eval_block,
    vals::{EvalError, StmtResult},
};

mod exprs;
pub mod isolate;
mod native_functions;
pub mod scope;
mod statements;
mod tests;
pub mod vals;

pub fn eval(program: &Program, isolate: Rc<RefCell<Isolate>>) -> Result<StmtResult, EvalError> {
    let root_scope = Rc::new(RefCell::new(Scope::new(isolate)));
    eval_block(&program.statements, &root_scope)
}
