use std::{cell::RefCell, convert::identity, rc::Rc};

use crate::parser::ast::{Expression, Statement};
use rua_identifiers::Identifier;

use super::{
    scope::Scope,
    vals::{EvalError, RuaVal, StmtResult},
};

impl Statement {
    pub fn eval(&self, env: Rc<RefCell<Scope>>) -> Result<StmtResult, EvalError> {
        match self {
            Self::Local(id, e) => Self::eval_local(id, e, &env),
            Self::Assign(id, e) => Self::eval_assign(id, e, &env),
            Self::Return(e) => Self::eval_return(e, env),
            Self::IfThen(cond, block) => {
                if cond.eval(env.clone())?.truthy() {
                    eval_block(block, &env)
                } else {
                    Ok(StmtResult::None)
                }
            }
            Self::IfThenElse(cond, if_true, if_false) => {
                if cond.eval(env.clone())?.truthy() {
                    eval_block(if_true, &env)
                } else {
                    eval_block(if_false, &env)
                }
            }
            Self::IfThenElseIf(v) => match v
                .iter()
                .try_find(|(cond, _)| Ok::<bool, EvalError>(cond.eval(env.clone())?.truthy()))?
            {
                Some((_, block)) => eval_block(block, &env),
                None => Ok(StmtResult::None),
            },
            Self::Call(expr, args) => {
                expr.callfn(args, &env)?;
                Ok(StmtResult::None)
            }
            Self::While(cond, block) => {
                while cond.eval(env.clone())?.truthy() {
                    match eval_block(block, &env)? {
                        r @ StmtResult::Return(_) => return Ok(r),
                        StmtResult::Break => return Ok(StmtResult::None),
                        StmtResult::None => {}
                    }
                }
                Ok(StmtResult::None)
            }
            Self::Break => Ok(StmtResult::Break),
        }
    }

    fn eval_return(
        expr: &Option<Box<Expression>>,
        env: Rc<RefCell<Scope>>,
    ) -> Result<StmtResult, EvalError> {
        Ok(match expr {
            Some(expr) => StmtResult::Return(expr.eval(env)?),
            None => StmtResult::Return(RuaVal::Nil),
        })
    }

    fn eval_local(
        ids: &[Identifier],
        exps: &[Expression],
        env: &Rc<RefCell<Scope>>,
    ) -> Result<StmtResult, EvalError> {
        let vals: Vec<_> = exps.iter().map(|e| e.eval(env.clone())).try_collect()?;
        for (i, id) in ids.iter().enumerate() {
            env.borrow_mut().set(*id, vals.get(i).cloned().map_or(RuaVal::Nil, identity));
        }

        Ok(StmtResult::None)
    }

    fn eval_assign(
        lhss: &[Expression],
        exps: &[Expression],
        env: &Rc<RefCell<Scope>>,
    ) -> Result<StmtResult, EvalError> {
        let vals: Vec<_> = exps.iter().map(|e| e.eval(env.clone())).try_collect()?;
        for (i, lhs) in lhss.iter().enumerate() {
            let val = vals.get(i).cloned().map_or(RuaVal::Nil, identity);
            match lhs {
                Expression::Identifier(id) => {
                    env.borrow_mut().update(*id, val);
                }
                Expression::Index(box (table_exp, idx)) => {
                    table_exp.eval(env.clone())?.into_table()?.insert(idx.eval(env.clone())?, val);
                }
                Expression::FieldAccess(table_exp, id) => {
                    table_exp.eval(env.clone())?.into_table()?.insert(
                        env.borrow()
                            .get_id_name(*id)
                            .expect("Got a non existing Identifier")
                            .into(),
                        val,
                    );
                }
                e => unreachable!("Invalid assignment LHS: {e:?}"),
            }
        }

        Ok(StmtResult::None)
    }
}

pub(super) fn eval_block(
    block: &[Statement],
    env: &Rc<RefCell<Scope>>,
) -> Result<StmtResult, EvalError> {
    for s in block {
        match s.eval(env.clone())? {
            v @ (StmtResult::Return(_) | StmtResult::Break) => return Ok(v),
            StmtResult::None => {}
        };
    }
    Ok(StmtResult::None)
}
