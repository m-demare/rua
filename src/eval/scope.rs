use std::{rc::Rc, cell::{RefCell, RefMut}, collections::hash_map::Entry};
use either::{Either, Left, Right};

use rustc_hash::FxHashMap;

use crate::{identifiers::{Identifier, Trie}, lex::tokens::TokenType};
use super::{vals::{LuaVal, NativeFunction}, native_functions};

pub struct Scope {
    store: FxHashMap<Identifier, LuaVal>,
    identifiers: Rc<RefCell<Trie>>,
    parent_or_globals: Either<Rc<RefCell<Scope>>, FxHashMap<Identifier, LuaVal>>,
}

impl Scope {
    pub fn new(identifiers: Rc<RefCell<Trie>>) -> Self {
        let mut globals = FxHashMap::default();
        globals.insert(
            insert_identifier(identifiers.borrow_mut(), "print"),
            LuaVal::NativeFunction(NativeFunction::new(Rc::new(native_functions::print))));

        Self { store: FxHashMap::default(), identifiers, parent_or_globals: Right(globals) }
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Self {
        Self { store: FxHashMap::default(),
            identifiers: parent.clone().borrow().identifiers(),
            parent_or_globals: Left(parent),
        }
    }

    pub fn set(&mut self, id: Identifier, val: LuaVal) {
        self.store.insert(id, val);
    }

    pub fn get(&self, id: Identifier) -> Option<LuaVal> {
        match self.store.get(&id) {
            Some(val) => Some(val.clone()),
            None => match &self.parent_or_globals {
                Left(parent) => parent.borrow().get(id),
                Right(globals) => globals.get(&id).cloned(),
            },
        }
    }

    pub fn get_id_name(&self, id: Identifier) -> Option<Box<str>> {
        self.identifiers.borrow().get(id)
    }

    pub fn identifiers(&self) -> Rc<RefCell<Trie>> {
        self.identifiers.clone()
    }
}

fn insert_identifier(mut identifiers: RefMut<Trie>, new_id: &str) -> Identifier {
    match identifiers.add_or_get(new_id) {
        TokenType::IDENTIFIER(id) => id,
        _ => unreachable!("Native function names are identifiers"),
    }
}

