use std::{
    array,
    hash::{BuildHasher, Hash, Hasher},
};

use nom::{
    branch::alt,
    character::complete::{alpha1, digit1, multispace0},
    combinator::eof,
    Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
    tag::complete::tag, ParserExt,
};

use crate::library::{Definitely, ITResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Minus,
    Assign(usize),
}

fn parse_command(input: &str) -> ITResult<&str, Command> {
    alt((
        tag("-").value(Command::Minus),
        digit1
            .parse_from_str_cut()
            .preceded_by(tag("="))
            .map(Command::Assign),
    ))
    .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Target<'a>(&'a str);

fn parse_target(input: &str) -> ITResult<&str, Target<'_>> {
    alpha1.map(Target).parse(input)
}

impl Hash for Target<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.0.as_bytes())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Step<'a> {
    raw: &'a str,
    target: Target<'a>,
    command: Command,
}

impl Hash for Step<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.raw.as_bytes())
    }
}

fn parse_step(input: &str) -> ITResult<&str, Step<'_>> {
    parse_target
        .and(parse_command)
        .with_recognized()
        .map(|(raw, (target, command))| Step {
            raw,
            target,
            command,
        })
        .parse(input)
}

#[derive(Debug)]
pub struct Input<'a> {
    steps: Vec<Step<'a>>,
}

fn parse_input(input: &str) -> ITResult<&str, Input<'_>> {
    collect_separated_terminated(parse_step, tag(","), multispace0.terminated(eof))
        .preceded_by(multispace0)
        .map(|steps| Input { steps })
        .parse(input)
}

impl<'a> TryFrom<&'a str> for Input<'a> {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

#[derive(Debug, Clone)]
struct Day15Hasher {
    state: u64,
}

#[derive(Default)]
struct Day15Hash;

impl Hasher for Day15Hasher {
    fn finish(&self) -> u64 {
        self.state
    }

    fn write(&mut self, bytes: &[u8]) {
        self.state = bytes.iter().copied().fold(self.state, |hash, byte| {
            hash.wrapping_add(byte as u64)
                .wrapping_mul(17)
                .wrapping_rem(256)
        })
    }
}

impl BuildHasher for Day15Hash {
    type Hasher = Day15Hasher;

    fn build_hasher(&self) -> Self::Hasher {
        Day15Hasher { state: 0 }
    }
}

pub fn part1(input: Input<'_>) -> Definitely<u64> {
    Ok(input
        .steps
        .iter()
        .map(|step| Day15Hash.hash_one(step))
        .sum())
}

#[derive(Debug, Clone)]
struct Day15HashMap<K, V, H = Day15Hash> {
    build_hash: H,
    slots: [Vec<(K, V)>; 256],
}

impl<K: Hash + Eq, V, H: BuildHasher + Default> Day15HashMap<K, V, H> {
    pub fn new() -> Self {
        Self {
            build_hash: H::default(),
            slots: array::from_fn(|_| Vec::new()),
        }
    }
}

impl<K: Hash + Eq, V, H: BuildHasher> Day15HashMap<K, V, H> {
    fn get_slot_mut(&mut self, key: &K) -> &mut Vec<(K, V)> {
        let hash = self.build_hash.hash_one(&key) as usize % self.slots.len();

        self.slots
            .get_mut(hash)
            .expect("already did a bounds check")
    }

    pub fn insert(&mut self, key: K, value: V) {
        let slot = self.get_slot_mut(&key);

        match slot
            .iter_mut()
            .find(|&&mut (ref current_key, _)| *current_key == key)
        {
            Some((_, current_value)) => *current_value = value,
            None => slot.push((key, value)),
        };
    }

    pub fn remove(&mut self, key: &K) {
        let slot = self.get_slot_mut(key);

        slot.retain(|(current_key, _value)| current_key != key)
    }
}

impl<K, H> Day15HashMap<K, usize, H> {
    pub fn score(&self) -> usize {
        self.slots
            .iter()
            .enumerate()
            .flat_map(|(slot_id, slot)| {
                slot.iter()
                    .enumerate()
                    .map(move |(lens_id, (_, lens))| (slot_id + 1) * (lens_id + 1) * lens)
            })
            .sum()
    }
}

pub fn part2(input: Input<'_>) -> Definitely<usize> {
    let mut map: Day15HashMap<Target<'_>, usize> = Day15HashMap::new();

    for step in &input.steps {
        match step.command {
            Command::Minus => map.remove(&step.target),
            Command::Assign(value) => map.insert(step.target, value),
        }
    }

    Ok(map.score())
}
