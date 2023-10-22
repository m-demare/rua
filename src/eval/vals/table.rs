use std::{
    cell::RefCell,
    convert::identity,
    hash::{BuildHasherDefault, Hash, Hasher},
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use rustc_hash::FxHashMap;

use super::RuaVal;

#[derive(Debug, Clone)]
pub struct Table {
    inner: Rc<RefCell<FxHashMap<RuaVal, RuaVal>>>,
    id: usize,
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);
impl Table {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(FxHashMap::default()).into(),
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: RefCell::new(FxHashMap::with_capacity_and_hasher(
                capacity,
                BuildHasherDefault::default(),
            ))
            .into(),
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
        }
    }

    pub fn insert(&mut self, key: RuaVal, val: RuaVal) -> Option<RuaVal> {
        self.inner.borrow_mut().insert(key, val)
    }

    pub fn get(&self, key: &RuaVal) -> RuaVal {
        self.inner.borrow().get(key).cloned().map_or(RuaVal::Nil, identity)
    }

    pub fn addr(&self) -> *const RefCell<FxHashMap<RuaVal, RuaVal>> {
        Rc::as_ptr(&self.inner)
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Table {}

impl Hash for Table {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
