use std::{
    borrow::Borrow,
    cmp::Reverse,
    collections::{btree_map, hash_map, BTreeMap, HashMap},
    hash::Hash,
    marker::PhantomData,
    num::NonZeroUsize,
};

use enum_map::{EnumArray, EnumMap};
use itertools::Itertools;

use super::try_build_iter;

/// The underlying store for the `Counter` type. Implements a simple key-value
/// store.
pub trait CounterStore: Sized {
    /// The item being stored & counted
    type Item;

    /// The type used during iteration over this store. For things like HashMap
    /// this is `&T`, but for things like EnumMap it's `T`
    type IterItem<'a>: Borrow<Self::Item>
    where
        Self: 'a;

    /// The iterator type used by this store. Stores will generally always
    /// iterate over references to the count, so for simplicity we reflect that
    /// here.
    type Iter<'a>: Iterator<Item = (Self::IterItem<'a>, &'a usize)> + Clone
    where
        Self::Item: 'a,
        Self: 'a;

    /// Create a new, empty Counter
    #[must_use]
    fn new() -> Self;

    /// Get the count for a given key
    #[must_use]
    fn get(&self, key: &Self::Item) -> usize;

    /// Add some amount to a given key
    fn add(&mut self, key: Self::Item, count: NonZeroUsize);

    /// Get how many unique keys with nonzero counts are in this store
    #[must_use]
    fn len(&self) -> usize;

    /// Iterate over all keys and their counts. This might include keys with
    /// a count of 0.
    #[must_use]
    fn iter(&self) -> Self::Iter<'_>;

    /// Remove all items from this store
    fn clear(&mut self);
}

impl<T: Hash + Eq> CounterStore for HashMap<T, usize> {
    type Item = T;
    type IterItem<'a> = &'a T where T: 'a;
    type Iter<'a> = hash_map::Iter<'a, T, usize> where T: 'a;

    #[inline]
    #[must_use]
    fn new() -> Self {
        HashMap::new()
    }

    #[inline]
    #[must_use]
    fn get(&self, key: &Self::Item) -> usize {
        self.get(key).copied().unwrap_or(0)
    }

    #[inline]
    fn add(&mut self, key: Self::Item, count: NonZeroUsize) {
        *self.entry(key).or_default() += count.get();
    }

    #[inline]
    #[must_use]
    fn len(&self) -> usize {
        self.len()
    }

    #[inline]
    #[must_use]
    fn iter(&self) -> Self::Iter<'_> {
        self.iter()
    }

    #[inline]
    fn clear(&mut self) {
        self.clear()
    }
}

impl<T: Ord> CounterStore for BTreeMap<T, usize> {
    type Item = T;
    type IterItem<'a> = &'a T where T: 'a;
    type Iter<'a> = btree_map::Iter<'a, T, usize> where T: 'a;

    #[inline]
    #[must_use]
    fn new() -> Self {
        BTreeMap::new()
    }

    #[inline]
    #[must_use]
    fn get(&self, key: &Self::Item) -> usize {
        self.get(key).copied().unwrap_or(0)
    }

    #[inline]
    fn add(&mut self, key: Self::Item, count: NonZeroUsize) {
        *self.entry(key).or_default() += count.get()
    }

    #[inline]
    #[must_use]
    fn len(&self) -> usize {
        self.len()
    }

    #[inline]
    #[must_use]
    fn iter(&self) -> Self::Iter<'_> {
        self.iter()
    }

    #[inline]
    fn clear(&mut self) {
        self.clear()
    }
}

impl<T: EnumArray<usize> + Copy> CounterStore for EnumMap<T, usize> {
    type Item = T;
    type IterItem<'a> = T where T: 'a;
    type Iter<'a> = enum_map::Iter<'a, T, usize> where T: 'a;

    #[inline]
    #[must_use]
    fn new() -> Self {
        Self::default()
    }

    #[inline]
    #[must_use]
    fn get(&self, key: &Self::Item) -> usize {
        self[*key]
    }

    #[inline]
    fn add(&mut self, key: Self::Item, count: NonZeroUsize) {
        self[key] += count.get()
    }

    #[inline]
    #[must_use]
    fn len(&self) -> usize {
        self.iter().filter(|(_, &count)| count > 0).count()
    }

    #[inline]
    #[must_use]
    fn iter(&self) -> Self::Iter<'_> {
        self.iter()
    }

    #[inline]
    fn clear(&mut self) {
        self.clear()
    }
}

/// A type to count things
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Counter<T, Store: CounterStore<Item = T> = HashMap<T, usize>> {
    counts: Store,
    phantom: PhantomData<T>,
}

impl<T, Store: CounterStore<Item = T>> Counter<T, Store> {
    /// Create a new, empty counter
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            counts: Store::new(),
            phantom: PhantomData,
        }
    }

    /// Get the number of items in the collection
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.counts.len()
    }

    /// Test if there's at least one of `value` in the store
    #[inline]
    #[must_use]
    pub fn contains(&self, value: &Store::Item) -> bool {
        self.get(value) > 0
    }

    /// Get the count for a given value
    #[inline]
    #[must_use]
    pub fn get(&self, value: &Store::Item) -> usize {
        self.counts.get(value)
    }

    /// Iterate over all of the items that this counter has at least one of
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Store::IterItem<'_>> + Clone {
        self.iter().map(|(item, _count)| item)
    }

    /// Iterate over all the items that this counter has at least one of, and
    /// their counts
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Store::IterItem<'_>, NonZeroUsize)> + Clone {
        self.counts
            .iter()
            .filter_map(|(item, &count)| NonZeroUsize::new(count).map(|count| (item, count)))
    }

    /// Add some items to the counter.
    #[inline]
    pub fn add(&mut self, item: Store::Item, count: NonZeroUsize) {
        self.counts.add(item, count)
    }

    /// Remove all items from this counter
    #[inline]
    pub fn clear(&mut self) {
        self.counts.clear()
    }

    /// Return an array of the `N` most plentiful items in the `Counter`. They
    /// are guaranteed to be sorted from most to least plentiful. Returns `None`
    /// if there are fewer than `N` unique items in the Counter.
    #[must_use]
    pub fn top<const N: usize>(&self) -> Option<[(Store::IterItem<'_>, NonZeroUsize); N]> {
        let mut iter = self.iter();
        let mut buffer = try_build_iter(&mut iter)?;

        if N > 1 {
            buffer.sort_unstable_by_key(|&(_, count)| Reverse(count));
        }

        iter.for_each(|(item, count)| {
            if let Some(last) = buffer.last_mut() {
                if last.1 < count {
                    *last = (item, count);

                    if N > 1 {
                        buffer.sort_unstable_by_key(|&(_, count)| Reverse(count));
                    }
                }
            }
        });

        Some(buffer)
    }

    /// Get a reference to the underlying store backing this Counter.
    #[inline]
    #[must_use]
    pub fn store(&self) -> &Store {
        &self.counts
    }
}

impl<T, Store: CounterStore<Item = T>> Default for Counter<T, Store> {
    #[inline]
    #[must_use]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq, Store: CounterStore<Item = T>> Extend<(T, NonZeroUsize)> for Counter<T, Store> {
    #[inline]
    fn extend<I: IntoIterator<Item = (T, NonZeroUsize)>>(&mut self, iter: I) {
        iter.into_iter()
            .coalesce(|(lhs, lhs_count), (rhs, rhs_count)| match lhs == rhs {
                true => Ok((
                    lhs,
                    lhs_count
                        .checked_add(rhs_count.get())
                        .expect("addition overflow"),
                )),
                false => Err(((lhs, lhs_count), (rhs, rhs_count))),
            })
            .for_each(|(item, count)| self.add(item, count))
    }
}

impl<T: Eq, Store: CounterStore<Item = T>> Extend<(T, usize)> for Counter<T, Store> {
    #[inline]
    fn extend<I: IntoIterator<Item = (T, usize)>>(&mut self, iter: I) {
        self.extend(
            iter.into_iter()
                .filter_map(|(item, count)| NonZeroUsize::new(count).map(|count| (item, count))),
        )
    }
}

impl<T: Eq, Store: CounterStore<Item = T>> Extend<T> for Counter<T, Store> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.extend(
            iter.into_iter()
                .map(|item| (item, NonZeroUsize::new(1).expect("one is not zero"))),
        )
    }
}

impl<T, Store: CounterStore<Item = T>, U> FromIterator<U> for Counter<T, Store>
where
    Self: Extend<U>,
{
    fn from_iter<I: IntoIterator<Item = U>>(iter: I) -> Self {
        let mut this = Self::new();
        this.extend(iter);
        this
    }
}

pub type HashCounter<T> = Counter<T, HashMap<T, usize>>;
pub type EnumCounter<T> = Counter<T, EnumMap<T, usize>>;
