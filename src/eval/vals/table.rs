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

impl Table {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
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
        if RuaVal::Nil == val {
            return self.inner.borrow_mut().remove(&key);
        }
        self.inner.borrow_mut().insert(key, val)
    }

    pub fn get(&self, key: &RuaVal) -> RuaVal {
        self.inner.borrow().get(key).cloned().map_or(RuaVal::Nil, identity)
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn push(&mut self, val: RuaVal) {
        let pos = self.arr_size() + 1;
        self.insert(RuaVal::Number((pos as f64).into()), val);
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn pop(&mut self) -> Option<RuaVal> {
        let pos = self.arr_size();
        self.remove(&RuaVal::Number((pos as f64).into()))
    }

    pub fn remove(&mut self, key: &RuaVal) -> Option<RuaVal> {
        self.inner.borrow_mut().remove(key)
    }

    pub fn addr(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn arr_size(&self) -> usize {
        let inner = self.inner.borrow();
        // TODO use a more efficient algorithm
        for i in 1.. {
            let val = inner.get(&RuaVal::Number((i as f64).into()));
            if val.is_none() {
                return i - 1;
            }
        }
        unreachable!();
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

impl<I> FromIterator<(I, RuaVal)> for Table
where
    I: Into<RuaVal>,
{
    fn from_iter<T: IntoIterator<Item = (I, RuaVal)>>(iter: T) -> Self {
        let mut table = Self::new();
        for (key, val) in iter {
            table.insert(key.into(), val);
        }
        table
    }
}
