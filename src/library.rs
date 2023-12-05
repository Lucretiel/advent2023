#![allow(dead_code)]

pub mod dynamic;

use std::{
    cmp::Reverse, collections::HashMap, convert::Infallible, hash::Hash, iter::FusedIterator, mem,
    ops::ControlFlow,
};

use brownstone::move_builder::{ArrayBuilder, PushResult};
use nom::{error::ParseError, Parser};
use nom_supreme::{error::ErrorTree, tag::TagError};

use self::dynamic::SubtaskStore;

#[macro_export]
macro_rules! express {
    ($receiver:ident $(.$method:ident($($args:tt)*))*) => {
        {
            let mut receiver = $receiver;
            $(
                receiver.$method($($args)*);
            )*
            receiver
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Counter<T: Hash + Eq> {
    counts: HashMap<T, usize>,
}

impl<T: Hash + Eq> Counter<T> {
    pub fn new() -> Self {
        Self {
            counts: HashMap::new(),
        }
    }

    pub fn with_capacity(size: usize) -> Self {
        Self {
            counts: HashMap::with_capacity(size),
        }
    }

    pub fn len(&self) -> usize {
        self.counts.len()
    }

    pub fn contains(&self, value: &T) -> bool {
        self.counts.contains(value)
    }

    pub fn get(&self, value: &T) -> usize {
        self.counts.get(value).copied().unwrap_or(0)
    }

    pub fn items(&self) -> impl Iterator<Item = &T> + FusedIterator + ExactSizeIterator + Clone {
        self.counts.keys()
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&T, usize)> + FusedIterator + ExactSizeIterator + Clone {
        self.counts.iter().map(|(key, &count)| (key, count))
    }

    pub fn add(&mut self, item: T, count: usize) {
        if count > 0 {
            *self.counts.entry(item).or_insert(0) += count
        }
    }

    pub fn top<const N: usize>(&self) -> Option<[(&T, usize); N]> {
        let mut iter = self.counts.iter().map(|(key, &value)| (key, value));
        let mut buffer = try_build_iter(&mut iter)?;
        buffer.sort_unstable_by_key(|&(_, count)| Reverse(count));

        iter.for_each(|(item, count)| {
            let Some(last) = buffer.last_mut() else {
                return;
            };
            if last.1 < count {
                *last = (item, count);

                buffer.sort_unstable_by_key(|&(_, count)| Reverse(count));
            }
        });

        Some(buffer)
    }
}

impl<T: Eq + Hash> Default for Counter<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Hash + Eq> Extend<(T, usize)> for Counter<T> {
    fn extend<I: IntoIterator<Item = (T, usize)>>(&mut self, iter: I) {
        iter.into_iter()
            .for_each(|(item, count)| self.add(item, count))
    }
}

impl<T: Hash + Eq> Extend<T> for Counter<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.extend(iter.into_iter().map(|item| (item, 1)))
    }
}

impl<T: Hash + Eq, U> FromIterator<U> for Counter<T>
where
    Self: Extend<U>,
{
    fn from_iter<I: IntoIterator<Item = U>>(iter: I) -> Self {
        let mut this = Self::new();
        this.extend(iter);
        this
    }
}

#[derive(Debug, Default, Clone)]
pub struct Chunks<I, const N: usize> {
    iterator: I,
}

impl<I: Iterator, const N: usize> Iterator for Chunks<I, N> {
    type Item = [I::Item; N];

    fn next(&mut self) -> Option<Self::Item> {
        Some(brownstone::build![self.iterator.next()?])
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (min, max) = self.iterator.size_hint();

        (min / N, max.map(|max| max / N))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.iterator.count() / N
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let builder = match ArrayBuilder::start() {
            PushResult::Full(array) => return Some(array),
            PushResult::NotFull(builder) => builder,
        };

        let n = n.checked_mul(N).expect("usize overflow");

        let mut builder = match builder.push(self.iterator.nth(n)?) {
            PushResult::Full(array) => return Some(array),
            PushResult::NotFull(builder) => builder,
        };

        loop {
            builder = match builder.push(self.iterator.next()?) {
                PushResult::Full(array) => return Some(array),
                PushResult::NotFull(builder) => builder,
            }
        }
    }

    fn fold<B, F>(self, init: B, mut func: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        let builder = match ArrayBuilder::start() {
            PushResult::Full(_array) => panic!("called Chunks::fold but N is 0"),
            PushResult::NotFull(builder) => builder,
        };

        let (_, accum) =
            self.iterator
                .fold((builder, init), |(builder, accum), item| {
                    match builder.push(item) {
                        PushResult::NotFull(builder) => (builder, accum),
                        PushResult::Full(array) => match ArrayBuilder::start() {
                            PushResult::Full(_arr) => unreachable!(),
                            PushResult::NotFull(builder) => (builder, func(accum, array)),
                        },
                    }
                });

        accum
    }
}

impl<T: FusedIterator, const N: usize> FusedIterator for Chunks<T, N> {}

impl<T: ExactSizeIterator, const N: usize> ExactSizeIterator for Chunks<T, N> {
    fn len(&self) -> usize {
        self.iterator.len() / N
    }
}

pub trait IterExt: Iterator + Sized {
    fn streaming_chunks<const N: usize>(self) -> Chunks<Self, N> {
        Chunks { iterator: self }
    }

    fn streaming_windows<const N: usize>(self) -> Windows<Self, N> {
        Windows {
            state: State::Begin,
            iter: self,
        }
    }

    fn disgorge_error<T, E>(self, destination: &mut Result<(), E>) -> DisgorgeError<'_, Self, E>
    where
        Self: Iterator<Item = Result<T, E>>,
    {
        DisgorgeError {
            iterator: self,
            error: destination,
        }
    }
}

impl<T: Iterator + Sized> IterExt for T {}

#[macro_export]
macro_rules! parser {
    (
        $(
            $parser:expr $(=> $bind:ident)?
        ),* ;
        $map:expr
    ) => {
        move |input| -> nom::IResult<_, _, _> {
            $(
                let (input, value) = $parser.parse(input)?;
                $(
                    let $bind = value;
                    let value = ();
                )?
                let _ = value;
            )*

            Ok((input, $map))
        }
    };
}

#[derive(Debug, Clone, Copy)]
enum State<T, const N: usize> {
    Begin,
    Buffered([T; N]),
    Done,
}

impl<T, const N: usize> State<T, N> {
    fn take(&mut self) -> Self {
        mem::replace(self, State::Done)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Windows<I: Iterator, const N: usize> {
    iter: I,
    state: State<I::Item, N>,
}

fn try_build_iter<I, const N: usize>(iter: I) -> Option<[I::Item; N]>
where
    I: IntoIterator,
{
    let builder = match ArrayBuilder::start() {
        PushResult::Full(array) => return Some(array),
        PushResult::NotFull(builder) => builder,
    };

    let result = iter
        .into_iter()
        .try_fold(builder, |builder, item| match builder.push(item) {
            PushResult::Full(array) => ControlFlow::Break(array),
            PushResult::NotFull(builder) => ControlFlow::Continue(builder),
        });

    match result {
        ControlFlow::Continue(_) => None,
        ControlFlow::Break(array) => Some(array),
    }
}

fn build_iter<I, const N: usize>(iter: I) -> [I::Item; N]
where
    I: IntoIterator,
{
    try_build_iter(iter).expect("iterator wasn't long enough")
}

impl<I: Iterator, const N: usize> Iterator for Windows<I, N>
where
    I::Item: Clone,
{
    type Item = [I::Item; N];

    fn next(&mut self) -> Option<Self::Item> {
        let buffer = match self.state.take() {
            State::Begin => try_build_iter(&mut self.iter)?,
            State::Buffered(buffer) => buffer,
            State::Done => return None,
        };

        if let Some(next) = self.iter.next() {
            self.state = State::Buffered(build_iter(buffer[1..].iter().cloned().chain([next])))
        }

        Some(buffer)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.state {
            State::Begin => {
                let (min, max) = self.iter.size_hint();
                (
                    min.saturating_sub(N - 1),
                    max.map(|max| max.saturating_sub(N - 1)),
                )
            }
            State::Buffered(_) => {
                let (min, max) = self.iter.size_hint();
                (
                    min.saturating_add(1),
                    max.and_then(|max| max.checked_add(1)),
                )
            }
            State::Done => (0, Some(0)),
        }
    }
}

impl<I: Iterator, const N: usize> FusedIterator for Windows<I, N> where I::Item: Clone {}

impl<I: ExactSizeIterator, const N: usize> ExactSizeIterator for Windows<I, N>
where
    I::Item: Clone,
{
    fn len(&self) -> usize {
        match self.state {
            State::Begin => self.iter.len().saturating_sub(N - 1),
            State::Buffered(_) => self.iter.len() + 1,
            State::Done => 0,
        }
    }
}

#[derive(Debug)]
pub struct DisgorgeError<'a, I, E> {
    iterator: I,
    error: &'a mut Result<(), E>,
}

impl<I, T, E> Iterator for DisgorgeError<'_, I, E>
where
    I: Iterator<Item = Result<T, E>>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.error.as_ref().ok()?;

        self.iterator
            .next()?
            .map_err(|err| {
                *self.error = Err(err);
            })
            .ok()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.error.is_err() {
            (0, Some(0))
        } else {
            let (_, max) = self.iterator.size_hint();
            (0, max)
        }
    }

    fn count(self) -> usize {
        self.iterator.filter(|item| item.is_ok()).count()
    }

    fn fold<B, F>(mut self, init: B, mut func: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        match *self.error {
            Err(_) => init,
            Ok(()) => match self.iterator.try_fold(init, |accum, item| match item {
                Ok(item) => ControlFlow::Continue(func(accum, item)),
                Err(err) => ControlFlow::Break((accum, err)),
            }) {
                ControlFlow::Continue(accum) => accum,
                ControlFlow::Break((accum, err)) => {
                    *self.error = Err(err);
                    accum
                }
            },
        }
    }
}

pub type Definitely<T> = Result<T, Infallible>;

pub trait ErrorWithLocation<I> {
    fn map_location(self, map: impl Fn(I) -> I) -> Self;
}

impl<I> ErrorWithLocation<I> for ErrorTree<I> {
    fn map_location(self, map: impl Fn(I) -> I) -> Self {
        self.map_locations(map)
    }
}

pub fn split_parser_fold<'i, 's, O, T, E>(
    mut item_parser: impl Parser<&'i str, O, E> + 's,
    separator: &'s str,
    mut init: impl FnMut() -> T + 's,
    mut fold: impl FnMut(T, O) -> T + 's,
) -> impl Parser<&'i str, T, E> + 's
where
    E: ErrorWithLocation<&'i str>,
    E: TagError<&'i str, &'s str>,
    E: ParseError<&'i str>,
{
    if separator.is_empty() {
        panic!("can't create a split parser with an empty separator")
    }

    move |mut input: &'i str| {
        let mut accum = init();

        loop {
            let (block, tail) = match input.split_once(separator) {
                None if input.is_empty() => return Ok(("", accum)),
                None => (input, ""),
                Some(pair) => pair,
            };

            let rebuild_tail = |local_tail_len: usize| {
                let rebuilt_tail_len = tail.len() + separator.len() + local_tail_len;
                let parsed_len = input.len() - rebuilt_tail_len;
                &input[parsed_len..]
            };

            let item = match item_parser.parse(block) {
                Ok(("", item)) => item,
                Ok((local_tail, _)) => {
                    return Err(nom::Err::Error(E::from_tag(
                        rebuild_tail(local_tail.len()),
                        separator,
                    )));
                }
                Err(nom::Err::Error(err)) => {
                    return Err(nom::Err::Error(
                        err.map_location(|local_tail| rebuild_tail(local_tail.len())),
                    ))
                }
                Err(nom::Err::Failure(err)) => {
                    return Err(nom::Err::Failure(
                        err.map_location(|local_tail| rebuild_tail(local_tail.len())),
                    ))
                }
                Err(nom::Err::Incomplete(_)) => {
                    return Err(nom::Err::Error(E::from_error_kind(
                        rebuild_tail(0),
                        nom::error::ErrorKind::Complete,
                    )))
                }
            };

            accum = fold(accum, item);
            input = tail;
        }
    }
}

pub fn split_parser<'i, 's, O, T, E>(
    item_parser: impl Parser<&'i str, O, E> + 's,
    separator: &'s str,
) -> impl Parser<&'i str, T, E> + 's
where
    E: ErrorWithLocation<&'i str>,
    E: TagError<&'i str, &'s str>,
    E: ParseError<&'i str>,
    T: Default + Extend<O> + 's,
{
    split_parser_fold(item_parser, separator, T::default, |collection, item| {
        express!(collection.extend([item]))
    })
}
