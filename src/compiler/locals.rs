use struct_invariant::invariant;

use crate::eval::vals::string::RuaString;

use super::bytecode::ParseError;

const MAX_LOCALS: u8 = u8::MAX;

pub(super) struct Local {
    name: RuaString,
    depth: usize,
    usable: bool,
    is_captured: bool,
}

pub(super) struct Locals {
    locals: Vec<Local>,
    scope_depth: usize,
}

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalHandle(u8);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalHandle(pub u8);

#[invariant(self.locals.len() <= u8::MAX.into(), "Pushed too many locals")]
impl Locals {
    pub fn new() -> Self {
        Self { locals: Vec::with_capacity(MAX_LOCALS as usize), scope_depth: 0 }
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope<F: FnMut(&Local)>(&mut self, mut applying: F) {
        self.scope_depth -= 1;

        let mut locals_in_scope = 0;
        for (i, local) in self.locals.iter().rev().enumerate() {
            if local.depth > self.scope_depth {
                locals_in_scope = i + 1;
                applying(local);
            } else {
                break;
            }
        }

        self.locals.truncate(self.locals.len() - locals_in_scope);
    }

    pub fn declare(&mut self, name: RuaString) -> Result<LocalHandle, ParseError> {
        if self.locals.len() == MAX_LOCALS as usize {
            return Err(ParseError::TooManyLocals);
        }
        // SAFETY: self.locals is only pushed to in `declare`, where
        // it is checked that it doesn't exceed u8::MAX
        let len = self.locals.len();
        let local = LocalHandle(unsafe { len.try_into().unwrap_unchecked() });
        self.locals.push(Local::new(name, self.scope_depth));
        Ok(local)
    }

    pub fn make_usable(&mut self, handle: LocalHandle) {
        self.locals[handle.0 as usize].usable = true;
    }

    pub fn resolve(&self, id: &RuaString) -> Option<LocalHandle> {
        // SAFETY: self.locals is only pushed to in `declare`, where
        // it is checked that it doesn't exceed u8::MAX
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.usable && &local.name == id)
            .map(|(i, _)| LocalHandle(unsafe { i.try_into().unwrap_unchecked() }))
    }

    pub fn drop(&mut self, n: usize) {
        debug_assert!(self.locals.len() >= n);
        self.locals.truncate(self.locals.len() - n);
    }

    pub fn get(&self, handle: LocalHandle) -> RuaString {
        self.locals[handle.0 as usize].name.clone()
    }

    #[must_use]
    #[allow(clippy::len_without_is_empty)]
    #[allow(clippy::cast_possible_truncation)]
    pub fn len(&self) -> u8 {
        self.locals.len() as u8
    }

    pub fn capture(&mut self, local: LocalHandle) {
        self.locals[local.0 as usize].is_captured = true;
    }

    pub fn name_of_reg(&self, reg: u8) -> Option<RuaString> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(i, _)| *i == reg.into())
            .map(|(_, local)| local.name.clone())
    }
}

impl LocalHandle {
    pub const fn pos(self) -> usize {
        self.0 as usize
    }
}

impl Local {
    const fn new(name: RuaString, depth: usize) -> Self {
        Self { name, depth, is_captured: false, usable: false }
    }

    pub const fn is_captured(&self) -> bool {
        self.is_captured
    }
}

impl From<LocalHandle> for u8 {
    fn from(value: LocalHandle) -> Self {
        value.0
    }
}

impl Default for Locals {
    fn default() -> Self {
        Self::new()
    }
}
