use std::{rc::Rc, cell::RefCell};

use crate::{parser::ast::{Expression, Statement}, identifiers::Identifier};

use super::{vals::{StmtResult, LuaVal, EvalError}, scope::Scope};

impl Statement {
    pub fn eval(&self, env: Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
        match self {
            Self::Local(id, e) => Self::eval_local(*id, e, &env),
            Self::Assign(id, e) => Self::eval_assign(*id, e, &env),
            Self::Return(e) => Self::eval_return(e, env),
            Self::IfThen(cond, block) => if cond.eval(env.clone())?.truthy() {
                    eval_block(block, &env)
                } else {
                    Ok(StmtResult::None)
                },
            Self::IfThenElse(cond, if_true, if_false) =>
                if cond.eval(env.clone())?.truthy() { eval_block(if_true, &env) }
                else { eval_block(if_false, &env) },
            Self::IfThenElseIf(v) => match v.iter().try_find(|(cond, _)| Ok::<bool, EvalError>(cond.eval(env.clone())?.truthy()))? {
                Some((_, block)) => eval_block(block, &env),
                None => Ok(StmtResult::None),
            },
            Self::Call(_, _) => todo!(),
            Self::While(cond, block) => {
                while cond.eval(env.clone())?.truthy() {
                    match eval_block(block, &env)? {
                        r @ StmtResult::Return(_) => return Ok(r),
                        StmtResult::Break => return Ok(StmtResult::None),
                        StmtResult::None => {},
                    }
                }
                Ok(StmtResult::None)
            },
            Self::Break => Ok(StmtResult::Break),
        }
    }

    fn eval_return(expr: &Option<Box<Expression>>, env: Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
        Ok(match expr {
            Some(expr) => StmtResult::Return(expr.eval(env)?),
            None => StmtResult::Return(LuaVal::Nil),
        })
    }

    fn eval_local(id: Identifier, exp: &Option<Box<Expression>>, env: &Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
        let val = exp.as_ref().map_or(Ok(LuaVal::Nil), |e| e.eval(env.clone()))?;
        env.borrow_mut().set(id, val);
        
        Ok(StmtResult::None)
    }

    fn eval_assign(id: Identifier, exp: &Expression, env: &Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
        let val = exp.eval(env.clone())?;
        env.borrow_mut().set(id, val);
        
        Ok(StmtResult::None)
    }
}

pub(super) fn eval_block(block: &[Statement], env: &Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
    for s in block {
        match s.eval(env.clone())? {
            v @ (StmtResult::Return(_) | StmtResult::Break) => return Ok(v),
            StmtResult::None => {},
        };
    }
    Ok(StmtResult::None)
}

