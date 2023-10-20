use std::{cell::RefCell, rc::Rc};

use crate::{eval::vals::Function, parser::ast::Expression};
use rua_identifiers::Identifier;

use super::{
    scope::Scope,
    vals::{EvalError, RuaResult, RuaVal},
};

impl Expression {
    #[allow(clippy::cast_precision_loss)]
    pub fn eval(&self, env: Rc<RefCell<Scope>>) -> RuaResult {
        use RuaVal as V;

        match self {
            Self::Identifier(id) => Self::get_identifier(*id, &env),
            Self::NumberLiteral(n) => Ok(V::Number(*n)),
            Self::BooleanLiteral(b) => Ok(V::Bool(*b)),
            Self::StringLiteral(s) => Ok(V::String(s.clone())),
            Self::Nil => Ok(V::Nil),
            Self::Not(e) => Ok(V::Bool(!e.eval(env)?.into_bool()?)),
            Self::Len(e) => Ok(V::Number(e.eval(env)?.into_str()?.len() as f64)),
            Self::Neg(e) => Ok(V::Number(-e.eval(env)?.try_into()?)),
            Self::Plus(box (e1, e2)) => {
                Ok(V::Number(e1.eval(env.clone())?.into_number()? + e2.eval(env)?.into_number()?))
            }
            Self::Minus(box (e1, e2)) => {
                Ok(V::Number(e1.eval(env.clone())?.into_number()? - e2.eval(env)?.into_number()?))
            }
            Self::Times(box (e1, e2)) => {
                Ok(V::Number(e1.eval(env.clone())?.into_number()? * e2.eval(env)?.into_number()?))
            }
            Self::Div(box (e1, e2)) => {
                Ok(V::Number(e1.eval(env.clone())?.into_number()? / e2.eval(env)?.into_number()?))
            }
            Self::Mod(box (e1, e2)) => {
                Ok(V::Number(e1.eval(env.clone())?.into_number()? % e2.eval(env)?.into_number()?))
            }
            Self::Exp(box (e1, e2)) => Ok(V::Number(
                e1.eval(env.clone())?.into_number()?.powf(e2.eval(env)?.into_number()?),
            )),
            Self::Eq(box (e1, e2)) => Ok(V::Bool(e1.eval(env.clone())? == e2.eval(env)?)),
            Self::Neq(box (e1, e2)) => Ok(V::Bool(e1.eval(env.clone())? != e2.eval(env)?)),
            Self::Le(box (e1, e2)) => {
                Ok(V::Bool(e1.eval(env.clone())?.into_number()? <= e2.eval(env)?.into_number()?))
            }
            Self::Ge(box (e1, e2)) => {
                Ok(V::Bool(e1.eval(env.clone())?.into_number()? >= e2.eval(env)?.into_number()?))
            }
            Self::Lt(box (e1, e2)) => {
                Ok(V::Bool(e1.eval(env.clone())?.into_number()? < e2.eval(env)?.into_number()?))
            }
            Self::Gt(box (e1, e2)) => {
                Ok(V::Bool(e1.eval(env.clone())?.into_number()? > e2.eval(env)?.into_number()?))
            }
            Self::And(box (e1, e2)) => e1.and(e2, env),
            Self::Or(box (e1, e2)) => e1.or(e2, env),
            Self::Dotdot(box (e1, e2)) => Ok(V::String(
                (e1.eval(env.clone())?.into_str()?.to_string() + &*e2.eval(env)?.into_str()?)
                    .into(),
            )),
            Self::Function(args, body) => {
                Ok(V::Function(Function::new(args.clone(), body.clone(), env)))
            }
            Self::Call(expr, args) => expr.callfn(args, &env),
            Self::FieldAccess(_, _) => todo!(),
        }
    }

    fn and(&self, rhs: &Self, env: Rc<RefCell<Scope>>) -> RuaResult {
        let lhs = self.eval(env.clone())?;
        if lhs.truthy() {
            rhs.eval(env)
        } else {
            Ok(lhs)
        }
    }

    fn or(&self, rhs: &Self, env: Rc<RefCell<Scope>>) -> RuaResult {
        let lhs = self.eval(env.clone())?;
        if lhs.truthy() {
            Ok(lhs)
        } else {
            rhs.eval(env)
        }
    }

    fn get_identifier(id: Identifier, env: &Rc<RefCell<Scope>>) -> RuaResult {
        match env.borrow().get(id) {
            Some(val) => Ok(val),
            None => Err(EvalError::UnknownId(
                env.borrow().get_id_name(id).expect("Got a non existing Identifier"),
            )),
        }
    }

    pub fn callfn(&self, args: &[Self], env: &Rc<RefCell<Scope>>) -> RuaResult {
        let val = self.eval(env.clone())?;
        let func = val.as_func()?;
        func.call(args, env)
    }
}
