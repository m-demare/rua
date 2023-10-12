use std::{rc::Rc, cell::RefCell};

use rustc_hash::FxHashMap;

use crate::identifiers::{Identifier, Trie};
use super::vals::LuaVal;

pub struct Scope {
    store: FxHashMap<Identifier, LuaVal>,
    identifiers: Rc<RefCell<Trie>>,
    parent: Option<Rc<RefCell<Scope>>>
}

impl Scope {
    pub fn new(identifiers: Rc<RefCell<Trie>>) -> Self {
        Self { store: FxHashMap::default(), identifiers, parent: None }
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Self {
        Self { store: FxHashMap::default(), identifiers: parent.clone().borrow().identifiers(), parent: Some(parent) }
    }

    pub fn set(&mut self, id: Identifier, val: LuaVal) {
        self.store.insert(id, val);
    }

    pub fn get(&self, id: Identifier) -> Option<LuaVal> {
        match self.store.get(&id) {
            Some(val) => Some(val.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(id),
                None => None,
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

