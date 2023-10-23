use super::{
    native_functions::default_global,
    vals::{table::Table, RuaVal},
};

use rua_identifiers::{Identifier, Trie};

use crate::lex::tokens::TokenType;

pub struct Isolate {
    identifiers: Trie<TokenType>,
    global: Table,
}

impl Isolate {
    pub fn with_global(identifiers: Trie<TokenType>, mut global: Table) -> Self {
        global.insert("_G".into(), RuaVal::Table(global.clone()));
        Self { identifiers, global }
    }

    pub fn new(identifiers: Trie<TokenType>) -> Self {
        let global = default_global();
        Self::with_global(identifiers, global)
    }

    pub fn identifiers_mut(&mut self) -> &mut Trie<TokenType> {
        &mut self.identifiers
    }

    pub const fn identifiers(&self) -> &Trie<TokenType> {
        &self.identifiers
    }

    pub fn get_global_id(&self, id: Identifier) -> RuaVal {
        let name = self.identifiers.get(id).expect("Got a non existing Identifier");
        self.global.get(&name.into())
    }

    pub fn set_global_id(&mut self, id: Identifier, val: RuaVal) {
        let name = self.identifiers.get(id).expect("Got a non existing Identifier");
        self.global.insert(name.into(), val);
    }
}
