#![allow(clippy::module_name_repetitions)]

use std::{fmt::Display, hash, ops::Deref, rc::Rc};

struct StringInner {
    data: Rc<[u8]>,
    hash: u64,
}

#[derive(Clone)]
pub struct RuaString(Rc<StringInner>);

impl hash::Hash for RuaString {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl hash::Hash for StringInner {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl PartialEq for RuaString {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0.data, &other.0.data)
    }
}

impl Eq for RuaString {}

impl RuaString {
    pub(crate) fn new(data: Rc<[u8]>, hash: u64) -> Self {
        Self(Rc::new(StringInner { data, hash }))
    }

    pub(crate) fn inner(&self) -> Rc<[u8]> {
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
