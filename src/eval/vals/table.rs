use std::{
    cell::RefCell,
    hash::{BuildHasherDefault, Hash, Hasher},
};

use rustc_hash::FxHashMap;

use super::RuaVal;

#[derive(Debug)]
struct TableInner {
    map: FxHashMap<RuaVal, RuaVal>,
    marked: bool,
}

#[derive(Debug)]
pub struct Table {
    inner: RefCell<TableInner>,
}

const MAX_SAFE_INTEGER: usize = 2usize.pow(53) - 1; // 2^53 – 1

impl Table {
    pub(crate) fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: RefCell::new(TableInner {
                map: FxHashMap::with_capacity_and_hasher(capacity, BuildHasherDefault::default()),
                marked: false,
            }),
        }
    }

    pub fn insert(&self, key: RuaVal, val: RuaVal) -> Option<RuaVal> {
        if val.is_nil() {
            return self.inner.borrow_mut().map.remove(&key);
        }
        self.inner.borrow_mut().map.insert(key, val)
    }

    pub fn get(&self, key: &RuaVal) -> Option<RuaVal> {
        self.inner.borrow().map.get(key).cloned()
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn push(&self, val: RuaVal) {
        let pos = self.arr_size() + 1;
        self.insert((pos as f64).into(), val);
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn pop(&self) -> Option<RuaVal> {
        let pos = self.arr_size();
        self.remove(&(pos as f64).into())
    }

    pub fn remove(&self, key: &RuaVal) -> Option<RuaVal> {
        self.inner.borrow_mut().map.remove(key)
    }

    pub fn clear(&self) {
        self.inner.borrow_mut().map.clear();
    }

    pub fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    /// Returns any number n, such that:
    /// - n is a key in the table
    /// - n+1 isn't a key in the table
    #[allow(clippy::cast_precision_loss)]
    pub fn arr_size(&self) -> usize {
        let map = &self.inner.borrow().map;
        let (mut lower, mut upper) = (0, 1);
        while self.get(&(upper as f64).into()).is_some() {
            lower = upper;
            if upper > MAX_SAFE_INTEGER / 2 {
                // Malicious input, resort to linear search
                for i in 1.. {
                    let val = map.get(&(i as f64).into());
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

    pub(crate) fn mark(&self) {
        {
            let already_marked = self.inner.borrow().marked;
            if already_marked {
                return;
            }
        }

        #[cfg(test)]
        println!("Marked {}", self.addr());

        {
            self.inner.borrow_mut().marked = true;
        }

        for (k, v) in &self.inner.borrow().map {
            k.mark();
            v.mark();
        }
    }
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Table {}

impl Hash for Table {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::addr_of!(*self).hash(state);
    }
}

impl<I> FromIterator<(I, RuaVal)> for Table
where
    I: Into<RuaVal>,
{
    fn from_iter<T: IntoIterator<Item = (I, RuaVal)>>(iter: T) -> Self {
        let table = Self::new();
        for (key, val) in iter {
            table.insert(key.into(), val);
        }
        table
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::vals::{IntoRuaVal, RuaVal, RuaValInner},
        eval::Vm,
    };

    use super::Table;

    #[test]
    fn test_insert() {
        let mut vm = Vm::new();
        let table = Table::new();
        table.insert(1.0.into(), b"test".into_rua(&mut vm));
        table.insert(3.0.into(), b"test2".into_rua(&mut vm));
        assert!(table.arr_size() == 1 || table.arr_size() == 3);

        let table1: RuaVal = Table::new().into();
        table.insert(table1.clone(), 5.0.into());
        let table2: RuaVal = Table::new().into();
        table.insert(table2.clone(), 7.0.into());
        let table3: RuaVal = Table::new().into();
        table.insert(table3.clone(), 9.0.into());
        table.push(true.into());

        assert_eq!(table.arr_size(), 3);
        assert_eq!(table.get(&1.0.into()), Some(b"test".into_rua(&mut vm)));
        assert_eq!(table.get(&2.0.into()), Some(true.into()));
        assert_eq!(table.get(&3.0.into()), Some(b"test2".into_rua(&mut vm)));
        assert_eq!(table.get(&table2), Some(7.0.into()));
        assert_eq!(table.get(&table1), Some(5.0.into()));
        assert_eq!(table.get(&table3), Some(9.0.into()));
        assert_eq!(table.get(&Table::new().into()), None);
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_remove() {
        let mut vm = Vm::new();
        let vec: Vec<(RuaVal, RuaVal)> = vec![
            (0.0.into(), 50.0.into()),
            (1.0.into(), 51.0.into()),
            (2.0.into(), 52.0.into()),
            (b"hello".into_rua(&mut vm), b"world".into_rua(&mut vm)),
            (3.0.into(), 53.0.into()),
            (4.0.into(), 54.0.into()),
            (6.0.into(), 56.0.into()),
        ];
        let table = Table::from_iter(vec);

        assert!(table.arr_size() == 4 || table.arr_size() == 6);
        match table.pop() {
            Some(RuaVal(RuaValInner::Number(n))) if n.val() == 56.0 => {
                assert_eq!(table.arr_size(), 4);
            }
            Some(RuaVal(RuaValInner::Number(n))) if n.val() == 54.0 => {
                assert!(table.arr_size() == 3 || table.arr_size() == 6);
            }
            Some(n) => panic!("Should have popped 54 or 56, not {n}"),
            None => panic!("There were items to pop"),
        };

        assert_eq!(table.remove(&b"foo".into_rua(&mut vm)), None);
        assert_eq!(table.remove(&b"hello".into_rua(&mut vm)), Some(b"world".into_rua(&mut vm)));
        assert_eq!(table.get(&b"hello".into_rua(&mut vm)), None);

        table.pop();
        assert!(table.arr_size() == 2 || table.arr_size() == 3 || table.arr_size() == 6);
        table.remove(&6.0.into());

        match table.get(&3.0.into()) {
            Some(_) => assert_eq!(table.arr_size(), 3),
            None => assert_eq!(table.arr_size(), 2),
        }
    }
}
