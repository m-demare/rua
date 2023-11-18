use either::Either;

use super::{bytecode::ParseError, locals::LocalHandle};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Upvalue {
    parent: Either<LocalHandle, UpvalueHandle>,
}

#[derive(Default)]
pub struct Upvalues {
    upvalues: Vec<Upvalue>,
}

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueHandle(u8);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueHandle(pub u8);

impl Upvalues {
    pub fn find_or_add(
        &mut self,
        upvalue: Either<LocalHandle, UpvalueHandle>,
    ) -> Result<UpvalueHandle, ParseError> {
        if let Some((i, _)) = self.upvalues.iter().enumerate().find(|(_, up)| up.parent == upvalue)
        {
            // SAFETY: self.upvalues is only pushed to in `find_or_add`, where
            // it is checked that it doesn't exceed u8::MAX
            return Ok(UpvalueHandle(unsafe { i.try_into().unwrap_unchecked() }));
        }
        let len = self.len();
        if len == u8::MAX {
            return Err(ParseError::TooManyLocals);
        }
        let up = Upvalue { parent: upvalue };
        self.upvalues.push(up);
        Ok(UpvalueHandle(len))
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u8 {
        // SAFETY: self.upvalues is only pushed to in `find_or_add`, where
        // it is checked that it doesn't exceed u8::MAX
        unsafe { self.upvalues.len().try_into().unwrap_unchecked() }
    }
}

impl Upvalue {
    pub const fn get(self) -> Either<LocalHandle, UpvalueHandle> {
        self.parent
    }
}

impl UpvalueHandle {
    pub const fn get(self) -> u8 {
        self.0
    }
}

impl IntoIterator for Upvalues {
    type Item = Upvalue;

    type IntoIter = std::vec::IntoIter<Upvalue>;

    fn into_iter(self) -> Self::IntoIter {
        self.upvalues.into_iter()
    }
}
