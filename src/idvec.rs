use nonmax::NonMaxUsize;
use std::{
    fmt::Debug,
    hash::Hash,
    iter::FusedIterator,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

macro_rules! new_id_type {
    ($vis:vis struct $name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        $vis struct $name(::nonmax::NonMaxUsize);

        impl $crate::idvec::Id for $name {
            fn from_index(index: ::nonmax::NonMaxUsize) -> Self {
                Self(index)
            }

            fn into_index(self) -> ::nonmax::NonMaxUsize {
                self.0
            }
        }

        impl ::core::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                write!(f, "{}({})", ::core::stringify!($name), self.0)
            }
        }
    };
}
pub(crate) use new_id_type;

pub trait Id: Copy {
    fn from_index(index: NonMaxUsize) -> Self;
    fn into_index(self) -> NonMaxUsize;
}

#[derive(Clone)]
#[repr(transparent)]
pub struct IdVec<I: Id, T> {
    values: Vec<T>,
    _index: PhantomData<I>,
}

impl<I: Id, T> IdVec<I, T> {
    pub const fn new() -> Self {
        Self::from_vec(Vec::new())
    }

    pub const fn from_vec(values: Vec<T>) -> Self {
        Self {
            values,
            _index: PhantomData,
        }
    }

    pub fn into_vec(self) -> Vec<T> {
        self.values
    }

    pub fn push_with(&mut self, f: impl FnOnce(I) -> T) -> I {
        let id = Id::from_index(self.len().try_into().unwrap());
        self.values.push(f(id));
        id
    }

    pub fn push(&mut self, value: T) -> I {
        self.push_with(|_| value)
    }
}

impl<I: Id, T> Default for IdVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Id, T> Deref for IdVec<I, T> {
    type Target = IdSlice<I, T>;

    fn deref(&self) -> &Self::Target {
        IdSlice::from_slice(&self.values)
    }
}

impl<I: Id, T> DerefMut for IdVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        IdSlice::from_slice_mut(&mut self.values)
    }
}

impl<I: Id + Debug, T: Debug> Debug for IdVec<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

#[repr(transparent)]
pub struct IdSlice<I: Id, T> {
    _index: PhantomData<I>,
    values: [T],
}

impl<I: Id, T> IdSlice<I, T> {
    pub const fn from_slice(values: &[T]) -> &IdSlice<I, T> {
        unsafe { &*(values as *const [T] as *const IdSlice<I, T>) }
    }

    pub const fn as_slice(&self) -> &[T] {
        &self.values
    }

    pub const fn from_slice_mut(values: &mut [T]) -> &mut IdSlice<I, T> {
        unsafe { &mut *(values as *mut [T] as *mut IdSlice<I, T>) }
    }

    pub const fn as_slice_mut(&mut self) -> &mut [T] {
        &mut self.values
    }

    pub const fn len(&self) -> usize {
        self.values.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, id: I) -> Option<&T> {
        self.values.get(id.into_index().get())
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.values.get_mut(id.into_index().get())
    }

    pub fn get_disjoint_mut<const N: usize>(&mut self, ids: [I; N]) -> Option<[&mut T; N]> {
        self.values
            .get_disjoint_mut(ids.map(|id| id.into_index().get()))
            .ok()
    }

    pub fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = (I, &T)> + DoubleEndedIterator + FusedIterator {
        self.values
            .iter()
            .enumerate()
            .map(|(index, value)| (I::from_index(index.try_into().unwrap()), value))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (I, &mut T)> + DoubleEndedIterator + FusedIterator {
        self.values
            .iter_mut()
            .enumerate()
            .map(|(index, value)| (I::from_index(index.try_into().unwrap()), value))
    }
}

impl<I: Id, T> Index<I> for IdSlice<I, T> {
    type Output = T;

    fn index(&self, id: I) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<I: Id, T> IndexMut<I> for IdSlice<I, T> {
    fn index_mut(&mut self, id: I) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<I: Id + Debug, T: Debug> Debug for IdSlice<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

pub struct IdMap<I: Id, T> {
    values: Vec<Option<T>>,
    _index: PhantomData<I>,
}

impl<I: Id, T> IdMap<I, T> {
    pub const fn new() -> Self {
        Self {
            values: Vec::new(),
            _index: PhantomData,
        }
    }

    pub const fn len(&self) -> usize {
        self.values.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn insert(&mut self, id: I, value: T) -> Option<T> {
        let index = id.into_index().get();
        if index >= self.len() {
            self.values.reserve(index - self.len() + 1);
            self.values.resize_with(index, || None);
            self.values.push(Some(value));
            None
        } else {
            self.values[index].replace(value)
        }
    }

    pub fn get(&self, id: I) -> Option<&T> {
        self.values
            .get(id.into_index().get())
            .and_then(|r| r.as_ref())
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.values
            .get_mut(id.into_index().get())
            .and_then(|r| r.as_mut())
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (I, &T)> + FusedIterator {
        self.values.iter().enumerate().filter_map(|(index, value)| {
            Some((I::from_index(index.try_into().unwrap()), value.as_ref()?))
        })
    }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (I, &mut T)> + FusedIterator {
        self.values
            .iter_mut()
            .enumerate()
            .filter_map(|(index, value)| {
                Some((I::from_index(index.try_into().unwrap()), value.as_mut()?))
            })
    }

    pub fn get_disjoint_mut<const N: usize>(&mut self, ids: [I; N]) -> [Option<&mut T>; N] {
        self.values
            .get_disjoint_mut(ids.map(|id| id.into_index().get()))
            .ok()
            .map(|refs| refs.map(Option::as_mut))
            .unwrap_or([const { None }; N])
    }
}

impl<I: Id, T> Default for IdMap<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Id + Eq + Hash, T> Index<I> for IdMap<I, T> {
    type Output = T;

    fn index(&self, id: I) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<I: Id + Eq + Hash, T> IndexMut<I> for IdMap<I, T> {
    fn index_mut(&mut self, id: I) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<I: Id + Debug, T: Debug> Debug for IdMap<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}
