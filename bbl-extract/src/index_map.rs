use std::{marker::PhantomData, ops::{Index, IndexMut}};
use ustr::{UstrMap, Ustr};

pub trait IndexMapKey {
    fn get(&self) -> usize;
}

pub struct UstrIndexMap<T, K: IndexMapKey> {
    storage: Vec<T>,
    map: UstrMap<usize>,
    phantom: PhantomData<K>,
}

impl<T, K> Default for UstrIndexMap<T, K>
where
    K: IndexMapKey,
{
    fn default() -> Self {
        UstrIndexMap::<T, K>::new()
    }
}

impl<T, K> UstrIndexMap<T, K>
where
    K: IndexMapKey,
{
    pub fn new() -> UstrIndexMap<T, K> {
        UstrIndexMap {
            storage: Vec::new(),
            map: Default::default(),
            phantom: PhantomData,
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.storage.iter()
    }

    pub fn get(&self, key: &Ustr) -> Option<&T> {
        self.map.get(key).map(|id| &self.storage[*id])
    }

    pub fn get_mut(&mut self, key: &Ustr) -> Option<&mut T> {
        self.map.get(key).map(|id| &mut self.storage[*id])
    }

    pub fn get_id(&self, key: &Ustr) -> Option<&usize> {
        self.map.get(key)
    }

    pub fn len(&self) -> usize {
        self.storage.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn insert(&mut self, key: Ustr, value: T) -> usize {
        let id = self.storage.len();
        self.storage.push(value);
        self.map.insert(key, id);
        id
    }
}

impl<T, K> Index<K> for UstrIndexMap<T, K>
where
    K: IndexMapKey,
{
    type Output = T;

    fn index(&self, index: K) -> &Self::Output {
        &self.storage[index.get()]
    }
}

impl<T, K> IndexMut<K> for UstrIndexMap<T, K>
where
    K: IndexMapKey,
{
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.storage[index.get()]
    }
}

