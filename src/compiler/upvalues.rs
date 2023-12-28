use either::Either;
use struct_invariant::invariant;

use super::{bytecode::ParseError, locals::LocalHandle};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Upvalue {
    location: Either<LocalHandle, UpvalueHandle>,
}

#[derive(Default, Debug)]
pub(crate) struct Upvalues {
    upvalues: Vec<Upvalue>,
}

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueHandle(u8);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueHandle(pub u8);

#[invariant(self.upvalues.len() <= u8::MAX.into(), "Pushed too many upvalues")]
impl Upvalues {
    pub(super) fn find_or_add(
        &mut self,
        upvalue: Either<LocalHandle, UpvalueHandle>,
    ) -> Result<UpvalueHandle, ParseError> {
        if let Some((i, _)) =
            self.upvalues.iter().enumerate().find(|(_, up)| up.location == upvalue)
        {
            return Ok(UpvalueHandle(i.try_into().expect("Upvalues shouldn't exceed u8::MAX")));
        }
        let len = self.len();
        if len == u8::MAX {
            return Err(ParseError::TooManyLocals);
        }
        let up = Upvalue { location: upvalue };
        self.upvalues.push(up);
        Ok(UpvalueHandle(len))
    }

    #[allow(clippy::len_without_is_empty)]
    pub(crate) fn len(&self) -> u8 {
        self.upvalues.len().try_into().expect("Upvalues shouldn't exceed u8::MAX")
    }
}

impl Upvalue {
    pub(crate) const fn location(self) -> Either<LocalHandle, UpvalueHandle> {
        self.location
    }
}

impl UpvalueHandle {
    pub(crate) const fn pos(self) -> usize {
        self.0 as usize
    }
}

impl IntoIterator for Upvalues {
    type Item = Upvalue;

    type IntoIter = std::vec::IntoIter<Upvalue>;

    fn into_iter(self) -> Self::IntoIter {
        self.upvalues.into_iter()
    }
}
