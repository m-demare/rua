use std::{
    collections::hash_map::RawEntryMut,
    hash::{self, BuildHasher, Hash, Hasher},
    rc::{Rc, Weak},
};

use ahash::{HashMap, HashMapExt};

// HashSet doesn't have the raw_entry_mut API
#[allow(clippy::zero_sized_map_values)]
#[derive(Debug)]
pub struct WeakInterner<T: hash::Hash + ?Sized + PartialEq> {
    map: HashMap<WeakKey<T>, ()>,
}

impl<T: hash::Hash + ?Sized + PartialEq> Default for WeakInterner<T> {
    fn default() -> Self {
        Self { map: HashMap::with_capacity(1 << 8) }
    }
}

impl<T: hash::Hash + ?Sized + PartialEq + std::fmt::Debug> WeakInterner<T> {
    pub fn insert_or_get(&mut self, key: Rc<T>) -> (Rc<T>, u64) {
        if self.map.len() >= self.map.capacity() {
            self.clean_weaks();
            self.reserve();
        }
        let mut state = self.map.hasher().build_hasher();
        key.hash(&mut state);
        let key_hash = state.finish();

        let mut state = self.map.hasher().build_hasher();
        WeakKey::new(key_hash, Rc::downgrade(&key)).hash(&mut state);
        let weak_key_hash = state.finish();

        let entry = self
            .map
            .raw_entry_mut()
            .from_hash(weak_key_hash, |weak_key| Some(&key) == weak_key.upgrade().as_ref());
        let res = match entry {
            RawEntryMut::Occupied(e) => e.key().upgrade().expect("Found key cannot be None"),
            RawEntryMut::Vacant(e) => {
                let weak_key = WeakKey::new(key_hash, Rc::downgrade(&key));
                e.insert(weak_key, ());
                key
            }
        };
        (res, key_hash)
    }

    fn clean_weaks(&mut self) {
        self.map.retain(|k, _| k.upgrade().is_some());
    }

    fn reserve(&mut self) {
        const SHRINK_FACTOR: usize = 4;
        const EXTRA_SPACE_FACTOR: usize = 4;
        const MIN_CAPACITY: usize = 1 << 8;

        let len = self.map.len();
        let capacity = self.map.capacity();
        if len * SHRINK_FACTOR < capacity {
            self.map.shrink_to(usize::max(len + len / EXTRA_SPACE_FACTOR, MIN_CAPACITY));
        } else {
            self.map.reserve(len);
        }
    }
}

#[derive(Debug)]
struct WeakKey<T: ?Sized> {
    hash: u64,
    key: Weak<T>,
}

impl<T: ?Sized> PartialEq for WeakKey<T> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.key, &other.key)
    }
}

impl<T: ?Sized> Eq for WeakKey<T> {}

impl<T: ?Sized> hash::Hash for WeakKey<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl<T: ?Sized> WeakKey<T> {
    fn new(hash: u64, key: Weak<T>) -> Self {
        Self { hash, key }
    }

    fn upgrade(&self) -> Option<Rc<T>> {
        self.key.upgrade()
    }
}
