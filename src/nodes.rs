use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{Index, IndexMut},
    process::abort,
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct NodeID<T>(NonZeroUsize, PhantomData<T>);

impl<T> Clone for NodeID<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for NodeID<T> {}

impl<T> PartialEq for NodeID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Eq for NodeID<T> {}

impl<T> PartialOrd for NodeID<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> Ord for NodeID<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for NodeID<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> Debug for NodeID<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NodeID").field(&self.0.get()).finish()
    }
}

impl<T> NodeID<T> {
    fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(1);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        NodeID(
            NonZeroUsize::new(id).unwrap_or_else(|| {
                // The counter has overflowed, this should never happen
                abort()
            }),
            PhantomData,
        )
    }
}

#[derive(Debug)]
pub struct Nodes<T> {
    nodes: HashMap<NodeID<T>, T>,
}

impl<T> Nodes<T> {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    pub fn add(&mut self, node: T) -> NodeID<T> {
        let id = NodeID::new();
        self.nodes.insert(id, node);
        id
    }

    pub fn add_deduplicate(&mut self, node: T, mut eq: impl FnMut(&T, &T) -> bool) -> NodeID<T> {
        self.nodes
            .iter()
            .find_map(|(&id, found)| eq(&node, found).then_some(id))
            .unwrap_or_else(|| self.add(node))
    }

    pub fn remove(&mut self, node: NodeID<T>) -> Option<T> {
        self.nodes.remove(&node)
    }

    pub fn get(&self, node: NodeID<T>) -> Option<&T> {
        self.nodes.get(&node)
    }

    pub fn get_mut(&mut self, node: NodeID<T>) -> Option<&mut T> {
        self.nodes.get_mut(&node)
    }
}

impl<T> Index<NodeID<T>> for Nodes<T> {
    type Output = T;

    fn index(&self, index: NodeID<T>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> IndexMut<NodeID<T>> for Nodes<T> {
    fn index_mut(&mut self, index: NodeID<T>) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<T> Default for Nodes<T> {
    fn default() -> Self {
        Self::new()
    }
}
