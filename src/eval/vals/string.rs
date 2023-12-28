#![allow(clippy::module_name_repetitions)]

use std::{
    fmt::Display,
    hash::{self, Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use rustc_hash::FxHasher;

use crate::eval::StringId;

struct StringInner {
    data: Rc<[u8]>,
    id: StringId,
    hash: u64,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RuaString(Rc<StringInner>);

impl Hash for StringInner {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
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
        let mut hasher = FxHasher::default();
        id.hash(&mut hasher);
        let hash = hasher.finish();
        Self(Rc::new(StringInner { data, id, hash }))
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
