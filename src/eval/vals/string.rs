// #![allow(clippy::module_name_repetitions)]

use std::{
    fmt::Display,
    hash::{self, Hash},
    ops::Deref,
    rc::Rc,
};

use crate::eval::StringId;

#[derive(Debug)]
struct StringInner {
    data: Rc<str>,
    id: StringId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub fn new(data: Rc<str>, id: StringId) -> Self {
        Self(Rc::new(StringInner { data, id }))
    }

    pub(super) fn inner(&self) -> Rc<str> {
        self.0.data.clone()
    }
}

impl Deref for RuaString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0.data
    }
}

impl Display for RuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.data)
    }
}

impl From<RuaString> for Rc<str> {
    fn from(val: RuaString) -> Self {
        val.0.data.clone()
    }
}
