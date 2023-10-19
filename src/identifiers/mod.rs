use std::{collections::HashMap, marker::PhantomData};

use crate::lex::tokens::{lookup_keyword, TokenType};

pub struct Trie {
    root: TrieNode,
    identifiers: HashMap<Identifier, Box<str>>,
    last_id: u32,
}

pub struct TrieNode {
    next: Vec<(char, Box<TrieNode>)>,
    // A HashMap is slower, there won't be that many keys
    // (see https://github.com/m-demare/rust_microbenches)
    val: Option<TokenType>,
}

pub struct TrieWalker<'trie> {
    curr_node: Option<&'trie TrieNode>,
    trie: PhantomData<&'trie Trie>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Copy)]
pub struct Identifier(pub u32);

impl<'trie> Trie {
    pub fn new() -> Self {
        Self { root: (TrieNode::new(None)), identifiers: HashMap::new(), last_id: 0 }
    }

    pub fn add_or_get(&'trie mut self, s: &str) -> TokenType {
        let mut node = &mut self.root;

        for ch in s.chars() {
            node = node.find_next_or_add(ch);
        }

        node.val
            .get_or_insert_with(|| {
                if let Some(token) = lookup_keyword(s) {
                    token
                } else {
                    self.last_id += 1;
                    self.identifiers.insert(Identifier(self.last_id), s.into());
                    TokenType::IDENTIFIER(Identifier(self.last_id))
                }
            })
            .clone()
    }

    pub fn get(&self, id: Identifier) -> Option<Box<str>> {
        self.identifiers.get(&id).cloned()
    }

    #[cfg(test)]
    pub fn find(&self, s: &str) -> Option<TokenType> {
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

impl Default for Trie {
    fn default() -> Self {
        Self::new()
    }
}

impl<'trie> TrieWalker<'trie> {
    pub const fn new(trie: &'trie Trie) -> TrieWalker<'trie> {
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

    pub fn get_res(self) -> Option<TokenType> {
        match self.curr_node {
            Some(n) => n.val.clone(),
            None => None,
        }
    }
}

impl<'trie> TrieNode {
    pub(self) fn new(val: Option<TokenType>) -> Self {
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

    #[test]
    fn test_identifiers() {
        let mut trie = Trie::new();
        assert_eq!(None, trie.find("foooo"));
        assert_eq!(None, trie.find("foo"));
        let foooo = trie.add_or_get("foooo");
        assert_eq!(TokenType::IDENTIFIER(Identifier(trie.last_id)), foooo);
        let foo = trie.add_or_get("foo");
        assert_eq!(TokenType::IDENTIFIER(Identifier(trie.last_id)), foo);

        assert_eq!(Some(foo), trie.find("foo"));
        assert_eq!(Some(foooo), trie.find("foooo"));
    }

    #[test]
    fn test_keywords() {
        let mut trie = Trie::new();
        assert_eq!(None, trie.find("local"));
        assert_eq!(None, trie.find("if"));

        let local = trie.add_or_get("local");
        assert_eq!(TokenType::LOCAL, local);

        let if_token = trie.add_or_get("if");
        assert_eq!(TokenType::IF, if_token);

        let loca = trie.add_or_get("loca");
        assert_eq!(TokenType::IDENTIFIER(Identifier(trie.last_id)), loca);

        let locall = trie.add_or_get("locall");
        assert_eq!(TokenType::IDENTIFIER(Identifier(trie.last_id)), locall);
    }

    #[test]
    fn test_walker() {
        let mut trie = Trie::new();

        let foo = trie.add_or_get("foo");
        let local = trie.add_or_get("local");

        let mut lukes_trie_walker = TrieWalker::new(&trie);
        "foo".chars().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(foo));

        lukes_trie_walker = TrieWalker::new(&trie);
        "local".chars().for_each(|ch| lukes_trie_walker.walk(ch));
        assert_eq!(lukes_trie_walker.get_res(), Some(local));
    }
}
