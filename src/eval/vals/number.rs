#![allow(clippy::module_name_repetitions)]

use std::hash::{self, Hash};

#[derive(Debug, PartialEq, Clone)]
pub struct RuaNumber(f64);

impl Hash for RuaNumber {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        // TODO this is not really safe to do,
        // should check out how other interpreters deal with it
        self.0.to_bits().hash(state);
    }
}

impl Eq for RuaNumber {}

impl RuaNumber {
    pub const fn new(val: f64) -> Self {
        Self(val)
    }

    pub const fn val(&self) -> f64 {
        self.0
    }
}