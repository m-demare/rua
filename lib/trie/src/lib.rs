#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![deny(unused_must_use)]
#![deny(clippy::mod_module_files)]

pub struct Trie<T> {
    root: TrieNode<T>,
}

pub struct TrieNode<T> {
    next: Vec<(u8, Box<TrieNode<T>>)>,
    // A HashMap is slower, there won't be that many keys
    // (see https://github.com/m-demare/rust_microbenches)
    val: Option<T>,
}

pub struct TrieWalker<'trie, T> {
    curr_node: Option<&'trie TrieNode<T>>,
}

impl<T> Trie<T> {
    pub const fn new() -> Self {
        Self { root: (TrieNode::new(None)) }
    }

    pub fn add_or_get(&mut self, s: &[u8], val: T) -> &T {
        let mut node = &mut self.root;

        for ch in s {
            node = node.find_next_or_add(*ch);
        }

        node.val.get_or_insert(val)
    }

    #[cfg(test)]
    pub fn find(&self, s: &[u8]) -> Option<&T> {
        let mut node = &self.root;
        for ch in s {
            node = &node.next.iter().find(|(c, _)| ch == c)?.1
        }
        node.val.as_ref()
    }
}

impl<T> Default for Trie<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'trie, T> TrieWalker<'trie, T> {
    pub const fn new(trie: &'trie Trie<T>) -> Self {
        Self { curr_node: Some(&trie.root) }
    }

    pub fn walk(&mut self, ch: u8) {
        if let Some(n) = &self.curr_node {
            self.curr_node = n.next.iter().find(|(c, _)| &ch == c).map(|(_, node)| node.as_ref());
        }
    }

    #[must_use]
    pub fn get_res(self) -> Option<&'trie T> {
        self.curr_node?.val.as_ref()
    }
}

impl<'trie, T> TrieNode<T> {
    pub(self) const fn new(val: Option<T>) -> Self {
        Self { next: Vec::new(), val }
    }

    fn find_next_or_add(&'trie mut self, ch: u8) -> &'trie mut Self {
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

    #[test]
    fn test_trie() {
        let mut trie = Trie::new();
        assert_eq!(None, trie.find(b"foooo"));
        assert_eq!(None, trie.find(b"foo"));
        let foooo = trie.add_or_get(b"foooo", 1).clone();
        assert_eq!(1, foooo);
        let foo = trie.add_or_get(b"foo", 2).clone();
        assert_eq!(2, foo);

        assert_eq!(Some(&foo), trie.find(b"foo"));
        assert_eq!(Some(&foooo), trie.find(b"foooo"));
    }

    #[test]
    fn test_walker() {
        let mut trie = Trie::new();

        let foo = trie.add_or_get(b"foo", true).clone();
        let local = trie.add_or_get(b"local", false).clone();

        let mut lukes_trie_walker = TrieWalker::new(&trie);
        "foo".bytes().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(&foo));

        lukes_trie_walker = TrieWalker::new(&trie);
        "local".bytes().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(&local));
    }
}
