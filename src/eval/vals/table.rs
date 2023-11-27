use std::{
    cell::RefCell,
    hash::{BuildHasherDefault, Hash, Hasher}, rc::Rc,
};

use rustc_hash::FxHashMap;

use crate::eval::{GcData, Vm, macros::trace_gc};

use super::{RuaVal, IntoRuaVal, RuaValInner};

#[derive(Debug)]
pub struct Table {
    map: RefCell<FxHashMap<RuaVal, RuaVal>>,
    marked: RefCell<bool>,
}

const MAX_SAFE_INTEGER: usize = 2usize.pow(53) - 1; // 2^53 – 1

impl Table {
    pub(crate) fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: RefCell::new(
                FxHashMap::with_capacity_and_hasher(capacity, BuildHasherDefault::default()),
            ),
            marked: RefCell::new(false)
        }
    }

    pub fn insert(&self, key: RuaVal, val: RuaVal) -> Option<RuaVal> {
        if val.is_nil() {
            return self.map.borrow_mut().remove(&key);
        }
        self.map.borrow_mut().insert(key, val)
    }

    pub fn get(&self, key: &RuaVal) -> Option<RuaVal> {
        self.map.borrow().get(key).cloned()
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
        self.map.borrow_mut().remove(key)
    }

    pub fn clear(&self) {
        self.map.borrow_mut().clear();
    }

    pub fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    /// Returns any number n, such that:
    /// - n is a key in the table
    /// - n+1 isn't a key in the table
    #[allow(clippy::cast_precision_loss)]
    pub fn arr_size(&self) -> usize {
        let map = &self.map.borrow();
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

    #[must_use]
    pub(in super::super) fn mark(&self) -> bool {
        let already_marked = self.marked.replace(true);
        trace_gc!("Marking table 0x{:x}. Was marked {}", self.addr(), already_marked);
        if already_marked {
            return false;
        }

        trace_gc!("Marked table 0x{:x}", self.addr());

        true
    }

    pub(super) fn blacken(&self, gc_data: &mut GcData) {
        let map: &FxHashMap<_, _> = &self.map.borrow();
        for (k, v) in map {
            k.mark(gc_data);
            v.mark(gc_data);
        }
    }

    pub(in super::super) fn soft_drop(&self) {
        trace_gc!("soft dropping table 0x{:x}", self.addr());
        self.map.borrow_mut().clear();
    }

    pub(in super::super) fn unmark(&self) -> bool {
        trace_gc!("Unmarking table 0x{:x}. Is marked? {}", self.addr(), *self.marked.borrow());
        self.marked.replace(false)
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

impl IntoRuaVal for Table {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        Rc::new(self).into_rua(vm)
    }
}

impl IntoRuaVal for Rc<Table> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        vm.register_table(self.clone());
        RuaVal(RuaValInner::Table(self))
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

        let table1: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table1.clone(), 5.0.into());
        let table2: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table2.clone(), 7.0.into());
        let table3: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table3.clone(), 9.0.into());
        table.push(true.into());

        assert_eq!(table.arr_size(), 3);
        assert_eq!(table.get(&1.0.into()), Some(b"test".into_rua(&mut vm)));
        assert_eq!(table.get(&2.0.into()), Some(true.into()));
        assert_eq!(table.get(&3.0.into()), Some(b"test2".into_rua(&mut vm)));
        assert_eq!(table.get(&table2), Some(7.0.into()));
        assert_eq!(table.get(&table1), Some(5.0.into()));
        assert_eq!(table.get(&table3), Some(9.0.into()));
        assert_eq!(table.get(&Table::new().into_rua(&mut vm)), None);
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
