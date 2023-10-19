use std::{cell::RefCell, rc::Rc};

use crate::parser::ast::Program;

use self::{
    scope::Scope,
    statements::eval_block,
    vals::{EvalError, StmtResult},
};

pub mod exprs;
mod native_functions;
pub mod scope;
mod statements;
mod tests;
pub mod vals;

pub fn eval(program: &Program, env: &Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
    eval_block(&program.statements, env)
}
