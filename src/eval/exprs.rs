use std::{cell::RefCell, rc::Rc};

use crate::{
    eval::vals::{function::Function, number::RuaNumber},
    parser::ast::{Expression, TableLiteral},
};
use rua_identifiers::Identifier;

use super::{
    scope::Scope,
    vals::{table::Table, RuaResult, RuaVal},
};

impl Expression {
    #[allow(clippy::cast_precision_loss)]
    pub fn eval(&self, env: Rc<RefCell<Scope>>) -> RuaResult {
        use RuaVal as V;

        match self {
            Self::Identifier(id) => Ok(Self::get_identifier(*id, &env)),
            Self::NumberLiteral(n) => Ok(V::Number(RuaNumber::new(*n))),
            Self::BooleanLiteral(b) => Ok(V::Bool(*b)),
            Self::StringLiteral(s) => Ok(V::String(s.clone())),
            Self::Nil => Ok(V::Nil),
            Self::Not(e) => Ok(V::Bool(!e.eval(env)?.into_bool()?)),
            Self::Len(e) => Ok(V::Number((e.eval(env)?.len()? as f64).into())),
            Self::Neg(e) => Ok(V::Number((-e.eval(env)?.into_number()?).into())),
            Self::Plus(box (e1, e2)) => Ok(V::Number(
                (e1.eval(env.clone())?.into_number()? + e2.eval(env)?.into_number()?).into(),
            )),
            Self::Minus(box (e1, e2)) => Ok(V::Number(
                (e1.eval(env.clone())?.into_number()? - e2.eval(env)?.into_number()?).into(),
            )),
            Self::Times(box (e1, e2)) => Ok(V::Number(
                (e1.eval(env.clone())?.into_number()? * e2.eval(env)?.into_number()?).into(),
            )),
            Self::Div(box (e1, e2)) => Ok(V::Number(
                (e1.eval(env.clone())?.into_number()? / e2.eval(env)?.into_number()?).into(),
            )),
            Self::Mod(box (e1, e2)) => Ok(V::Number(
                (e1.eval(env.clone())?.into_number()? % e2.eval(env)?.into_number()?).into(),
            )),
            Self::Exp(box (e1, e2)) => Ok(V::Number(
                e1.eval(env.clone())?.into_number()?.powf(e2.eval(env)?.into_number()?).into(),
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
            Self::TableLiteral(box TableLiteral(array_part, identifiers_part, map_part)) => {
                Self::table_literal(&env, array_part, identifiers_part, map_part)
            }
            Self::FieldAccess(table, id) => Self::field_access(env, table, *id),
            Self::Index(box (table, idx)) => Self::index(env, table, idx),
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

    fn get_identifier(id: Identifier, env: &Rc<RefCell<Scope>>) -> RuaVal {
        env.borrow().get(id)
    }

    pub fn callfn(&self, args: &[Self], env: &Rc<RefCell<Scope>>) -> RuaResult {
        let val = self.eval(env.clone())?;
        let func = val.as_func()?;

        let arg_vals: Vec<RuaVal> = args.iter().map(|arg| arg.eval(env.clone())).try_collect()?;

        func.call(&arg_vals)
    }

    fn field_access(env: Rc<RefCell<Scope>>, table: &Self, id: Identifier) -> RuaResult {
        let key = env.borrow().get_id_name(id).expect("Got a non existing Identifier").into();
        Ok(table.eval(env)?.into_table()?.get(&key))
    }

    #[allow(clippy::cast_precision_loss)]
    fn table_literal(
        env: &Rc<RefCell<Scope>>,
        array_part: &[Self],
        identifiers_part: &[(Identifier, Self)],
        map_part: &[(Self, Self)],
    ) -> RuaResult {
        let mut table = Table::with_capacity(array_part.len() + map_part.len());
        for (i, val) in array_part.iter().enumerate() {
            let idx = i as f64 + 1.0;
            table.insert(RuaVal::Number(RuaNumber::new(idx)), val.eval(env.clone())?);
        }

        for (id, val) in identifiers_part {
            table.insert(
                env.borrow().get_id_name(*id).expect("Got a non existing Identifier").into(),
                val.eval(env.clone())?,
            );
        }

        for (key, val) in map_part {
            table.insert(key.eval(env.clone())?, val.eval(env.clone())?);
        }
        Ok(RuaVal::Table(table))
    }

    pub fn index(env: Rc<RefCell<Scope>>, table: &Self, idx: &Self) -> RuaResult {
        let key = idx.eval(env.clone())?;
        Ok(table.eval(env)?.into_table()?.get(&key))
    }
}
