#![allow(clippy::module_name_repetitions)]

use std::hash::{self, Hash};

use super::RuaVal;

#[derive(Debug, PartialEq, Clone, Copy)]
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

    pub const fn val(self) -> f64 {
        self.0
    }
}

impl From<f64> for RuaNumber {
    fn from(val: f64) -> Self {
        Self::new(val)
    }
}

impl From<RuaNumber> for f64 {
    fn from(value: RuaNumber) -> Self {
        value.val()
    }
}

impl From<RuaNumber> for RuaVal {
    fn from(value: RuaNumber) -> Self {
        Self::Number(value)
    }
}

impl From<f64> for RuaVal {
    fn from(val: f64) -> Self {
        Self::Number(RuaNumber::new(val))
    }
}
