use either::{Either, Left, Right};
use std::{
    cell::{RefCell, RefMut},
    collections::hash_map::Entry,
    rc::Rc,
};

use rustc_hash::FxHashMap;

use super::{
    native_functions,
    vals::{NativeFunction, RuaVal},
};
use crate::lex::tokens::{lookup_keyword, TokenType};
use rua_identifiers::{Identifier, Trie};

pub struct Scope {
    store: FxHashMap<Identifier, RuaVal>,
    identifiers: Rc<RefCell<Trie<TokenType>>>,
    parent_or_globals: Either<Rc<RefCell<Scope>>, FxHashMap<Identifier, RuaVal>>,
}

impl Scope {
    pub fn new(identifiers: Rc<RefCell<Trie<TokenType>>>) -> Self {
        let mut globals = FxHashMap::default();
        globals.insert(
            insert_identifier(identifiers.borrow_mut(), "print"),
            RuaVal::NativeFunction(NativeFunction::new(Rc::new(native_functions::print))),
        );
        globals.insert(
            insert_identifier(identifiers.borrow_mut(), "tostring"),
            RuaVal::NativeFunction(NativeFunction::new(Rc::new(native_functions::tostring))),
        );
        globals.insert(
            insert_identifier(identifiers.borrow_mut(), "tonumber"),
            RuaVal::NativeFunction(NativeFunction::new(Rc::new(native_functions::tonumber))),
        );
        globals.insert(
            insert_identifier(identifiers.borrow_mut(), "type"),
            RuaVal::NativeFunction(NativeFunction::new(Rc::new(native_functions::rua_type))),
        );

        Self { store: FxHashMap::default(), identifiers, parent_or_globals: Right(globals) }
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            store: FxHashMap::default(),
            identifiers: parent.clone().borrow().identifiers(),
            parent_or_globals: Left(parent),
        }
    }

    pub fn set(&mut self, id: Identifier, val: RuaVal) {
        self.store.insert(id, val);
    }

    pub fn get(&self, id: Identifier) -> Option<RuaVal> {
        match self.store.get(&id) {
            Some(val) => Some(val.clone()),
            None => match &self.parent_or_globals {
                Left(parent) => parent.borrow().get(id),
                Right(globals) => globals.get(&id).cloned(),
            },
        }
    }

    pub fn update(&mut self, id: Identifier, val: RuaVal) {
        match self.store.entry(id) {
            Entry::Occupied(mut e) => {
                e.insert(val);
            }
            Entry::Vacant(_) => match &mut self.parent_or_globals {
                Left(parent) => parent.borrow_mut().update(id, val),
                Right(globals) => {
                    globals.insert(id, val);
                }
            },
        }
    }

    pub fn get_id_name(&self, id: Identifier) -> Option<Box<str>> {
        self.identifiers.borrow().get(id)
    }

    pub fn identifiers(&self) -> Rc<RefCell<Trie<TokenType>>> {
        self.identifiers.clone()
    }
}

fn insert_identifier(mut identifiers: RefMut<Trie<TokenType>>, new_id: &str) -> Identifier {
    let identifier = identifiers.add_or_get(new_id, |id, s| {
        if lookup_keyword(s).is_none() {
            TokenType::IDENTIFIER(id)
        } else {
            panic!("Cannot use {s} as an identifier, it's a reserved keyword")
        }
    });
    match identifier {
        TokenType::IDENTIFIER(id) => id,
        _ => panic!("Cannot use {new_id} as an identifier, it's a reserved keyword"),
    }
}
