use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::{Hash, Hasher},
    num::NonZeroU32,
    rc::Rc,
};

use nohash_hasher::BuildNoHashHasher;

use crate::eval::{macros::trace_gc, GcData, Vm};

use super::{EvalError, IntoRuaVal, RuaVal, RuaValInner};

#[derive(Debug)]
pub struct Table {
    map: RefCell<HashMap<RuaVal, RuaVal, BuildNoHashHasher<RuaVal>>>,
    array: RefCell<Vec<RuaVal>>,
    marked: Cell<bool>,
    vm_id: Cell<Option<NonZeroU32>>,
}

const MAX_SAFE_INTEGER: usize = 2usize.pow(53) - 1; // 2^53 – 1

const MAX_ARR_BITS: usize = std::mem::size_of::<usize>() * 8;

impl Table {
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacities(0, 0)
    }

    #[must_use]
    pub fn with_capacities(map_capacity: usize, array_capacity: usize) -> Self {
        let mut array = Vec::with_capacity(array_capacity);
        array.resize(array.capacity(), RuaVal::nil());
        Self {
            map: RefCell::new(HashMap::with_capacity_and_hasher(
                map_capacity,
                BuildNoHashHasher::default(),
            )),
            array: array.into(),
            marked: false.into(),
            vm_id: None.into(),
        }
    }

    /// # Errors
    ///
    /// Returns `InvalidTableIndex` if the key is `nil` or `NaN`
    pub fn insert(&self, key: RuaVal, val: RuaVal) -> Result<(), EvalError> {
        let new_key_idx = key.as_number().ok().and_then(try_into_usize);
        if let Some(n) = new_key_idx {
            let mut array = self.array.borrow_mut();
            if let Some(slot) = array.get_mut(n) {
                *slot = val;
                return Ok(());
            }
        }

        validate_key(&key)?;
        if val.is_nil() {
            self.map.borrow_mut().remove(&key);
            return Ok(());
        }

        let mut map = self.map.borrow_mut();
        if map.len() >= map.capacity() {
            // Recompute array/map sizes

            // nums[i] = number of keys 'k' where 2^(i - 1) < k <= 2^i
            let mut nums = [0; MAX_ARR_BITS];
            let mut arr = self.array.borrow_mut();

            let total_int_keys = Self::count_nums_arr(&mut nums, &arr)
                + Self::count_nums_map(&mut nums, &map)
                + new_key_idx.map_or(0, |n| {
                    Self::count_num(&mut nums, n);
                    1
                });

            let (opt_arr_size, _elems_in_opt_arr) =
                Self::compute_optimal_array(&nums, total_int_keys);
            Self::resize(opt_arr_size, &mut arr, &mut map);

            if let Some(n) = new_key_idx {
                if let Some(slot) = arr.get_mut(n) {
                    *slot = val;
                    return Ok(());
                }
            }
        }
        map.insert(key, val);
        Ok(())
    }

    pub fn get(&self, key: &RuaVal) -> Option<RuaVal> {
        let n = key.as_number().ok().and_then(try_into_usize);
        if let Some(n) = n {
            self.get_int(key, n)
        } else {
            self.get_generic(key)
        }
    }

    #[inline]
    fn get_int(&self, key: &RuaVal, int_key: usize) -> Option<RuaVal> {
        let array = self.array.borrow();
        if let Some(val) = array.get(int_key) {
            if val.is_nil() {
                None
            } else {
                Some(val.clone())
            }
        } else {
            self.get_generic(key)
        }
    }

    fn get_generic(&self, key: &RuaVal) -> Option<RuaVal> {
        self.map.borrow().get(key).cloned()
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn push(&self, val: RuaVal) {
        let pos = self.length() + 1;
        let _ = self.insert((pos as f64).into(), val);
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn pop(&self) -> Option<RuaVal> {
        let pos = self.length();
        self.remove(&(pos as f64).into())
    }

    pub fn remove(&self, key: &RuaVal) -> Option<RuaVal> {
        let new_key_idx = key.as_number().ok().and_then(try_into_usize);
        if let Some(n) = new_key_idx {
            let mut array = self.array.borrow_mut();
            if let Some(v) = array.get_mut(n) {
                return Some(std::mem::replace(v, RuaVal::nil()));
            }
        }
        self.map.borrow_mut().remove(key)
    }

    pub fn clear(&self) {
        self.map.borrow_mut().clear();
        self.array.borrow_mut().clear();
    }

    pub fn addr(&self) -> usize {
        std::ptr::addr_of!(*self) as usize
    }

    /// Returns any number n, such that:
    /// - n is a key in the table
    /// - n+1 isn't a key in the table
    #[allow(clippy::cast_precision_loss)]
    pub fn length(&self) -> usize {
        let (mut lower, mut upper) = (0, 1);
        while self.get(&(upper as f64).into()).is_some() {
            lower = upper;
            if upper > MAX_SAFE_INTEGER / 2 {
                // Malicious input, resort to linear search
                for i in 1.. {
                    let val = self.get(&(i as f64).into());
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
        for (k, v) in &*self.map.borrow() {
            k.mark(gc_data);
            v.mark(gc_data);
        }
        for v in &*self.array.borrow() {
            v.mark(gc_data);
        }
    }

    pub(in super::super) fn soft_drop(&self) {
        trace_gc!("soft dropping table 0x{:x}", self.addr());
        self.clear();
    }

    pub(in super::super) fn unmark(&self) -> bool {
        trace_gc!("Unmarking table 0x{:x}. Is marked? {}", self.addr(), self.marked.get());
        self.marked.replace(false)
    }

    pub(super) fn register_in(&self, vm_id: NonZeroU32) -> bool {
        let old_id = self.vm_id.replace(Some(vm_id));
        if let Some(old_id) = old_id {
            assert!(vm_id == old_id, "Cannot register table in a different Vm");
            false
        } else {
            true
        }
    }

    fn count_num(nums: &mut [u32; MAX_ARR_BITS], n: usize) {
        nums[n.checked_ilog2().map_or(0, |i| i + 1) as usize] += 1;
    }

    fn count_nums_arr(nums: &mut [u32; MAX_ARR_BITS], arr: &[RuaVal]) -> usize {
        let mut sum = 0;
        for (k, v) in arr.iter().enumerate() {
            if !v.is_nil() {
                Self::count_num(nums, k);
                sum += 1;
            }
        }
        sum
    }

    fn count_nums_map(
        nums: &mut [u32; MAX_ARR_BITS],
        map: &HashMap<RuaVal, RuaVal, BuildNoHashHasher<RuaVal>>,
    ) -> usize {
        let mut sum = 0;
        for k in map.keys() {
            let n = k.as_number().ok().and_then(try_into_usize);
            if let Some(n) = n {
                Self::count_num(nums, n);
                sum += 1;
            }
        }
        sum
    }

    // largest n such that:
    // * at least half the slots between 1 and n are in use (to avoid wasting space with sparse arrays)
    // * there is at least one used slot between n/2 + 1 and n (to avoid a size n when n/2 would do)
    #[must_use]
    fn compute_optimal_array(nums: &[u32; 64], total_int_keys: usize) -> (usize, usize) {
        let mut opt_arr_size: usize = 0;
        let mut elems_in_opt_arr: usize = 0;
        let mut acc: usize = 0;
        // #{keys < 2^i}

        for (i, num) in nums.iter().copied().enumerate() {
            if (1 << i) / 2 > total_int_keys {
                break;
            }
            let num = num as usize;

            acc += num;
            if num > 0 && acc >= (1 << i) / 2 {
                // more than half elems present?
                opt_arr_size = 1 << i;
                elems_in_opt_arr = acc;
            }
        }

        debug_assert!(
            opt_arr_size == 0 || opt_arr_size / 2 <= elems_in_opt_arr,
            "Optimal array cannot be more than half empty ({opt_arr_size}, {elems_in_opt_arr})"
        );
        debug_assert!(
            elems_in_opt_arr <= opt_arr_size,
            "Cannot have more elements in array than its size ({opt_arr_size}, {elems_in_opt_arr})"
        );
        (opt_arr_size, elems_in_opt_arr)
    }

    fn resize(
        opt_arr_size: usize,
        arr: &mut Vec<RuaVal>,
        map: &mut HashMap<RuaVal, RuaVal, BuildNoHashHasher<RuaVal>>,
    ) {
        const SHRINK_FACTOR: usize = 4;
        const EXTRA_SPACE_FACTOR: usize = 5;
        const MIN_GROWING_CAPACITY: usize = 8;

        if opt_arr_size <= arr.len() {
            // Dobule map capacity, plus enough to fit
            // the array elements that will be moved
            map.reserve(usize::max(MIN_GROWING_CAPACITY, (arr.len() - opt_arr_size) + map.len()));
            #[allow(clippy::cast_precision_loss)]
            arr.drain(opt_arr_size..arr.len())
                .enumerate()
                .filter(|(_, v)| !v.is_nil())
                .map(|(k, v)| (k + opt_arr_size, v))
                .for_each(|(k, v)| {
                    map.insert((k as f64).into(), v);
                });
            if opt_arr_size * SHRINK_FACTOR < arr.capacity() {
                arr.shrink_to(opt_arr_size + opt_arr_size / EXTRA_SPACE_FACTOR);
            }
        } else {
            arr.reserve(usize::max(MIN_GROWING_CAPACITY, opt_arr_size - arr.len()));
            arr.resize(arr.capacity(), RuaVal::nil());

            map.retain(|k, v| {
                let arr_key = k.as_number().ok().and_then(try_into_usize);
                if let Some(arr_key) = arr_key {
                    if let Some(slot) = arr.get_mut(arr_key) {
                        *slot = std::mem::replace(v, RuaVal::nil());
                        return false;
                    }
                }
                true
            });
            if map.len() * SHRINK_FACTOR < arr.capacity() {
                map.shrink_to(map.len() + map.len() / EXTRA_SPACE_FACTOR);
            }
        }
    }
}

fn validate_key(key: &RuaVal) -> Result<(), EvalError> {
    if key.is_nil() {
        return Err(EvalError::InvalidTableIndex(true));
    }
    if let Ok(f) = key.as_number() {
        if f.is_nan() {
            return Err(EvalError::InvalidTableIndex(false));
        }
    }
    Ok(())
}

impl Default for Table {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Table {
    #[inline]
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
        let iter = iter.into_iter();
        let table = Self::with_capacities(iter.size_hint().0, 0);
        for (key, val) in iter {
            table.insert(key.into(), val).expect("Table::from_iter with invalid keys");
        }
        table
    }
}

impl<I> FromIterator<I> for Table
where
    I: Into<RuaVal>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let table = Self::with_capacities(0, iter.size_hint().0);
        for (key, val) in iter.enumerate() {
            table
                .insert(
                    try_into_f64(key).expect("Array too large for Table::from_iter").into(),
                    val.into(),
                )
                .expect("Table::from_iter with invalid keys");
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
        if self.register_in(vm.id()) {
            vm.register_table(&self);
        }
        RuaVal(RuaValInner::Table(self))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::vals::{EvalError, IntoRuaVal, RuaVal, RuaValInner},
        eval::Vm,
    };

    use super::Table;

    #[test]
    fn test_insert() -> Result<(), EvalError> {
        let mut vm = Vm::new();
        let table = Table::new();
        table.insert(1.0.into(), b"test".into_rua(&mut vm))?;
        table.insert(3.0.into(), b"test2".into_rua(&mut vm))?;
        assert!(table.length() == 1 || table.length() == 3);

        let table1: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table1.clone(), 5.0.into())?;
        let table2: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table2.clone(), 7.0.into())?;
        let table3: RuaVal = Table::new().into_rua(&mut vm);
        table.insert(table3.clone(), 9.0.into())?;
        table.push(true.into());

        assert_eq!(table.length(), 3);
        assert_eq!(table.get(&1.0.into()), Some(b"test".into_rua(&mut vm)));
        assert_eq!(table.get(&2.0.into()), Some(true.into()));
        assert_eq!(table.get(&3.0.into()), Some(b"test2".into_rua(&mut vm)));
        assert_eq!(table.get(&table2), Some(7.0.into()));
        assert_eq!(table.get(&table1), Some(5.0.into()));
        assert_eq!(table.get(&table3), Some(9.0.into()));
        assert_eq!(table.get(&Table::new().into_rua(&mut vm)), None);
        Ok(())
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

        assert!(table.length() == 4 || table.length() == 6, "length was {}", table.length());
        match table.pop() {
            Some(RuaVal(RuaValInner::Number(n))) if n.val() == 56.0 => {
                assert_eq!(table.length(), 4, "length was {}", table.length());
            }
            Some(RuaVal(RuaValInner::Number(n))) if n.val() == 54.0 => {
                assert!(
                    table.length() == 3 || table.length() == 6,
                    "length was {}",
                    table.length()
                );
            }
            Some(n) => panic!("Should have popped 54 or 56, not {n}"),
            None => panic!("There were items to pop"),
        };

        assert_eq!(table.remove(&b"foo".into_rua(&mut vm)), None);
        assert_eq!(table.remove(&b"hello".into_rua(&mut vm)), Some(b"world".into_rua(&mut vm)));
        assert_eq!(table.get(&b"hello".into_rua(&mut vm)), None);

        table.pop();
        assert!(
            table.length() == 2 || table.length() == 3 || table.length() == 6,
            "length was {}",
            table.length()
        );
        table.remove(&6.0.into());
        assert_eq!(table.get(&6.0.into()), None);

        match table.get(&3.0.into()) {
            Some(_) => assert_eq!(table.length(), 3),
            None => assert_eq!(table.length(), 2),
        }
    }

    #[test]
    #[allow(clippy::cast_precision_loss)]
    fn test_array() {
        let table = Table::new();
        insert_all(&table, (1..200).filter(|i| i % 10 != 0));

        assert!(table.length() % 10 == 9 && table.length() < 200, "length was {}", table.length());
        assert!(table.get(&(table.length() as f64).into()).is_some());
        assert_eq!(table.get(&(1.0 + table.length() as f64).into()), None);
        assert_all_present(&table, (1..200).filter(|i| i % 10 != 0));

        // All elements should be in the array part
        assert_eq!(table.map.borrow().capacity(), 0);
        assert!(
            table.array.borrow().capacity() >= 200 - 20,
            "capacity was {}",
            table.array.borrow().capacity()
        );

        table.remove(&3.0.into());
        assert_eq!(table.get(&3.0.into()), None);
    }

    #[test]
    #[allow(clippy::cast_precision_loss)]
    fn test_sparse_array() {
        let table = Table::new();
        insert_all(&table, (1..200).filter(|i| i % 10 != 0).map(|i| (i + 50) * 4));

        assert!(table.length() == 0 || table.get(&(table.length() as f64).into()).is_some());
        assert_eq!(table.get(&(1.0 + table.length() as f64).into()), None);
        assert_all_present(&table, (1..200).filter(|i| i % 10 != 0).map(|i| (i + 50) * 4));

        // All elements should be in the map part
        assert!(
            table.map.borrow().capacity() >= 200 - 20,
            "capacity was {}",
            table.array.borrow().capacity()
        );
        assert_eq!(table.array.borrow().capacity(), 0);
    }

    #[test]
    #[allow(clippy::cast_precision_loss)]
    fn test_sparse_array_to_array_conversion() {
        let table = Table::new();
        insert_all(&table, 170..201);
        insert_all(&table, (0..50).rev());
        insert_all(&table, 50..170);

        assert_eq!(table.length(), 200);
        assert_all_present(&table, 0..201);

        // All elements should be in the array part, but map capacity may be non-zero
        assert!(
            table.map.borrow().capacity() < 100,
            "capacity was {}",
            table.map.borrow().capacity()
        );
        assert!(table.array.borrow().capacity() >= 200);
    }

    fn insert_all<I: Iterator<Item = u16>>(t: &Table, it: I) {
        it.map(f64::from).for_each(|i| t.insert(i.into(), i.into()).expect("All keys are valid"));
    }

    fn assert_all_present<I: Iterator<Item = u16>>(t: &Table, it: I) {
        it.map(f64::from).for_each(|i| assert!(t.get(&i.into()).is_some(), "{i} was not present"));
    }
}

#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::float_cmp
)]
#[inline]
fn try_into_usize(float: f64) -> Option<usize> {
    let n = float as usize;
    if n as f64 == float {
        Some(n)
    } else {
        None
    }
}

#[allow(clippy::cast_precision_loss)]
pub(crate) const fn try_into_f64(n: usize) -> Option<f64> {
    if n > MAX_SAFE_INTEGER {
        None
    } else {
        Some(n as f64)
    }
}
