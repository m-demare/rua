use std::{rc::Rc, cell::RefCell};

use crate::parser::ast::Program;

use self::{vals::{StmtResult, EvalError}, statements::eval_block, scope::Scope};

pub mod vals;
pub mod exprs;
pub mod scope;
mod statements;
mod tests;

pub fn eval(program: &Program, env: &Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
    eval_block(&program.statements, env)
}

