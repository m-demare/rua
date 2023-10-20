use std::{collections::HashMap, marker::PhantomData};

pub struct Trie<T: Clone> {
    root: TrieNode<T>,
    identifiers: HashMap<Identifier, Box<str>>, // TODO FxHashMap?
    last_id: u32,
}

pub struct TrieNode<T: Clone> {
    next: Vec<(char, Box<TrieNode<T>>)>,
    // A HashMap is slower, there won't be that many keys
    // (see https://github.com/m-demare/rust_microbenches)
    val: Option<T>,
}

pub struct TrieWalker<'trie, T: Clone> {
    curr_node: Option<&'trie TrieNode<T>>,
    trie: PhantomData<&'trie Trie<T>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Copy)]
pub struct Identifier(u32);

impl<'trie, T: Clone> Trie<T> {
    pub fn new() -> Self {
        Self { root: (TrieNode::new(None)), identifiers: HashMap::new(), last_id: 0 }
    }

    pub fn add_or_get<F: Fn(Identifier, &str) -> T>(&'trie mut self, s: &str, get_val: F) -> T {
        let mut node = &mut self.root;

        for ch in s.chars() {
            node = node.find_next_or_add(ch);
        }

        node.val
            .get_or_insert_with(|| {
                self.last_id += 1;
                self.identifiers.insert(Identifier(self.last_id), s.into());
                get_val(Identifier(self.last_id), s)
            })
            .clone()
    }

    pub fn get(&self, id: Identifier) -> Option<Box<str>> {
        self.identifiers.get(&id).cloned()
    }

    #[cfg(test)]
    pub fn find(&self, s: &str) -> Option<T> {
        let mut node = &self.root;
        for ch in s.chars() {
            match node.next.iter().find(|(c, _)| &ch == c) {
                Some((_, n)) => node = n,
                None => return None,
            }
        }
        node.val.clone()
    }
}

impl<T: Clone> Default for Trie<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'trie, T: Clone> TrieWalker<'trie, T> {
    pub const fn new(trie: &'trie Trie<T>) -> TrieWalker<'trie, T> {
        Self { curr_node: Some(&trie.root), trie: PhantomData }
    }

    pub fn walk(&mut self, ch: char) {
        if let Some(n) = &self.curr_node {
            self.curr_node = match n.next.iter().find(|(c, _)| &ch == c) {
                Some((_, found)) => Some(found),
                None => None,
            };
        }
    }

    pub fn get_res(self) -> Option<T> {
        match self.curr_node {
            Some(n) => n.val.clone(),
            None => None,
        }
    }
}

impl<'trie, T: Clone> TrieNode<T> {
    pub(self) fn new(val: Option<T>) -> Self {
        Self { next: Vec::new(), val }
    }

    fn find_next_or_add(&'trie mut self, ch: char) -> &'trie mut Self {
        if let Some(i) = self.next.iter().position(|(c, _)| &ch == c) {
            &mut self.next[i].1
        } else {
            self.next.push((ch, Box::new(Self::new(None))));
            &mut self.next.last_mut().expect("Just pushed").1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const fn identity2(id: Identifier, _: &str) -> Identifier {
        id
    }

    #[test]
    fn test_identifiers() {
        let mut trie = Trie::new();
        assert_eq!(None, trie.find("foooo"));
        assert_eq!(None, trie.find("foo"));
        let foooo = trie.add_or_get("foooo", identity2);
        assert_eq!(Identifier(trie.last_id), foooo);
        let foo = trie.add_or_get("foo", identity2);
        assert_eq!(Identifier(trie.last_id), foo);

        assert_eq!(Some(foo), trie.find("foo"));
        assert_eq!(Some(foooo), trie.find("foooo"));
    }

    #[test]
    fn test_walker() {
        let mut trie = Trie::new();

        let foo = trie.add_or_get("foo", identity2);
        let local = trie.add_or_get("local", identity2);

        let mut lukes_trie_walker = TrieWalker::new(&trie);
        "foo".chars().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(foo));

        lukes_trie_walker = TrieWalker::new(&trie);
        "local".chars().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(local));
    }
}
