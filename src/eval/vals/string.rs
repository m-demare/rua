#![allow(clippy::module_name_repetitions)]

use std::{
    fmt::Display,
    hash::{self, Hash},
    ops::Deref,
    rc::Rc,
};

use crate::eval::StringId;

struct StringInner {
    data: Rc<[u8]>,
    id: StringId,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RuaString(Rc<StringInner>);

impl Hash for StringInner {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for StringInner {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for StringInner {}

impl RuaString {
    pub(crate) fn new(data: Rc<[u8]>, id: StringId) -> Self {
        Self(Rc::new(StringInner { data, id }))
    }

    pub(super) fn inner(&self) -> Rc<[u8]> {
        self.0.data.clone()
    }
}

impl Deref for RuaString {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0.data
    }
}

impl Display for RuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = String::from_utf8_lossy(&self.0.data);
        write!(f, "{s}")
    }
}

impl std::fmt::Debug for RuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl From<RuaString> for Rc<[u8]> {
    fn from(val: RuaString) -> Self {
        val.0.data.clone()
    }
}
