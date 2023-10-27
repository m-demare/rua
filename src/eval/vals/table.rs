use std::{
    cell::RefCell,
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

const MAX_SAFE_INTEGER: usize = 2usize.pow(53) - 1; // 2^53 – 1

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

    pub fn get(&self, key: &RuaVal) -> Option<RuaVal> {
        self.inner.borrow().get(key).cloned()
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

    /// Returns any number n, such that:
    /// - n is a key in the table
    /// - n+1 isn't a key in the table
    #[allow(clippy::cast_precision_loss)]
    pub fn arr_size(&self) -> usize {
        let inner = self.inner.borrow();
        let (mut lower, mut upper) = (0, 1);
        while self.get(&(upper as f64).into()).is_some() {
            lower = upper;
            if upper > MAX_SAFE_INTEGER / 2 {
                // Malicious input, resort to linear search
                for i in 1.. {
                    let val = inner.get(&RuaVal::Number((i as f64).into()));
                    if val.is_none() {
                        return i - 1;
                    }
                }
            }
            upper *= 2;
        }
        while upper > lower + 1 {
            let mid = (lower + upper) / 2;
            match self.get(&(mid as f64).into()) {
                Some(_) => lower = mid,
                None => upper = mid,
            }
        }
        lower
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

#[cfg(test)]
mod tests {
    use crate::eval::vals::RuaVal;

    use super::Table;

    #[test]
    fn test_insert() {
        let mut table = Table::new();
        table.insert(1.0.into(), "test".into());
        table.insert(3.0.into(), "test2".into());
        assert!(table.arr_size() == 1 || table.arr_size() == 3);

        let table2: RuaVal = Table::new().into();
        table.insert(table2.clone(), 5.0.into());
        table.push(true.into());

        assert_eq!(table.arr_size(), 3);
        assert_eq!(table.get(&1.0.into()), Some("test".into()));
        assert_eq!(table.get(&2.0.into()), Some(true.into()));
        assert_eq!(table.get(&3.0.into()), Some("test2".into()));
        assert_eq!(table.get(&table2), Some(5.0.into()));
        assert_eq!(table.get(&Table::new().into()), None);
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_remove() {
        let vec: Vec<(RuaVal, RuaVal)> = vec![
            (0.0.into(), 50.0.into()),
            (1.0.into(), 51.0.into()),
            (2.0.into(), 52.0.into()),
            ("hello".into(), "world".into()),
            (3.0.into(), 53.0.into()),
            (4.0.into(), 54.0.into()),
            (6.0.into(), 56.0.into()),
        ];
        let mut table = Table::from_iter(vec);

        assert!(table.arr_size() == 4 || table.arr_size() == 6);
        match table.pop() {
            Some(RuaVal::Number(n)) if n.val() == 56.0 => assert_eq!(table.arr_size(), 4),
            Some(RuaVal::Number(n)) if n.val() == 54.0 => {
                assert!(table.arr_size() == 3 || table.arr_size() == 6);
            }
            Some(n) => panic!("Should have popped 54 or 56, not {n}"),
            None => panic!("There were items to pop"),
        };

        assert_eq!(table.remove(&"foo".into()), None);
        assert_eq!(table.remove(&"hello".into()), Some("world".into()));
        assert_eq!(table.get(&"hello".into()), None);

        table.pop();
        assert!(table.arr_size() == 2 || table.arr_size() == 3 || table.arr_size() == 6);
        table.remove(&6.0.into());

        match table.get(&3.0.into()) {
            Some(_) => assert_eq!(table.arr_size(), 3),
            None => assert_eq!(table.arr_size(), 2),
        }
    }
}
