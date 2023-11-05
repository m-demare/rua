use crate::eval::vals::string::RuaString;

use super::bytecode::ParseError;

const MAX_LOCALS: u8 = u8::MAX;

pub(super) struct Local {
    name: RuaString,
    depth: usize,
}

pub(super) struct Locals {
    locals: Vec<Local>,
    scope_depth: usize,
}

#[cfg(not(test))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalHandle(u8);

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalHandle(pub u8);

impl Locals {
    pub fn new() -> Self {
        Self { locals: Vec::with_capacity(MAX_LOCALS as usize), scope_depth: 0 }
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self) -> usize {
        self.scope_depth -= 1;

        let mut locals_in_scope = 0;
        for (i, local) in self.locals.iter().rev().enumerate() {
            if local.depth > self.scope_depth {
                locals_in_scope = i + 1;
            }
        }

        self.locals.truncate(self.locals.len() - locals_in_scope);
        locals_in_scope
    }

    pub fn declare(&mut self, name: RuaString) -> Result<(), ParseError> {
        if self.locals.len() == MAX_LOCALS as usize {
            return Err(ParseError::TooManyLocals);
        }
        self.locals.push(Local::new(name, self.scope_depth));
        Ok(())
    }

    pub fn resolve(&self, id: &RuaString) -> Option<LocalHandle> {
        // SAFETY: self.locals is only pushed to in `declare`, where
        // it is checked that it doesn't exceed u8::MAX
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| &local.name == id)
            .map(|(i, _)| LocalHandle(unsafe { i.try_into().unwrap_unchecked() }))
    }

    pub fn get(&self, handle: LocalHandle) -> RuaString {
        self.locals[handle.0 as usize].name.clone()
    }

    #[cfg(debug_assertions)]
    #[must_use]
    #[allow(clippy::len_without_is_empty)]
    #[allow(clippy::cast_possible_truncation)]
    pub fn len(&self) -> u8 {
        self.locals.len() as u8
    }
}

impl LocalHandle {
    pub const fn pos(&self) -> usize {
        self.0 as usize
    }
}

impl Local {
    pub(super) const fn new(name: RuaString, depth: usize) -> Self {
        Self { name, depth }
    }
}
