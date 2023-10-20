use crate::eval::native_functions::default_global;

use super::vals::RuaVal;

use rua_identifiers::{Identifier, Trie};
use rustc_hash::FxHashMap;

use crate::{lex::tokens::TokenType, rua_global};

pub struct Isolate {
    identifiers: Trie<TokenType>,
    global: FxHashMap<Identifier, RuaVal>,
}

impl Isolate {
    pub const fn with_global(
        identifiers: Trie<TokenType>,
        global: FxHashMap<Identifier, RuaVal>,
    ) -> Self {
        Self { identifiers, global }
    }

    pub fn new(mut identifiers: Trie<TokenType>) -> Self {
        let global = rua_global!(
            identifiers = &mut identifiers;
        );
        Self::with_global(identifiers, global)
    }

    pub fn identifiers_mut(&mut self) -> &mut Trie<TokenType> {
        &mut self.identifiers
    }

    pub const fn identifiers(&self) -> &Trie<TokenType> {
        &self.identifiers
    }

    pub fn get_global(&self, id: Identifier) -> Option<RuaVal> {
        self.global.get(&id).cloned()
    }

    pub fn set_global(&mut self, id: Identifier, val: RuaVal) {
        self.global.insert(id, val);
    }
}
