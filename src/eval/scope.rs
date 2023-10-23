use std::{cell::RefCell, collections::hash_map::Entry, rc::Rc};

use rustc_hash::FxHashMap;

use super::isolate::Isolate;
use super::vals::RuaVal;
use rua_identifiers::Identifier;

pub struct Scope {
    store: FxHashMap<Identifier, RuaVal>,
    isolate: Rc<RefCell<Isolate>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new(isolate: Rc<RefCell<Isolate>>) -> Self {
        Self { store: FxHashMap::default(), isolate, parent: None }
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            store: FxHashMap::default(),
            isolate: parent.clone().borrow().isolate(),
            parent: Some(parent),
        }
    }

    pub fn set(&mut self, id: Identifier, val: RuaVal) {
        self.store.insert(id, val);
    }

    pub fn get(&self, id: Identifier) -> RuaVal {
        match self.store.get(&id) {
            Some(val) => val.clone(),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(id),
                None => self.isolate.borrow().get_global_id(id),
            },
        }
    }

    pub fn update(&mut self, id: Identifier, val: RuaVal) {
        match self.store.entry(id) {
            Entry::Occupied(mut e) => {
                e.insert(val);
            }
            Entry::Vacant(_) => match &mut self.parent {
                Some(parent) => parent.borrow_mut().update(id, val),
                None => {
                    self.isolate.borrow_mut().set_global_id(id, val);
                }
            },
        }
    }

    pub fn get_id_name(&self, id: Identifier) -> Option<Box<str>> {
        self.isolate.borrow().identifiers().get(id)
    }

    fn isolate(&self) -> Rc<RefCell<Isolate>> {
        self.isolate.clone()
    }
}
