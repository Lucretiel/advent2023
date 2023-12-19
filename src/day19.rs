use std::{
    cmp::{self, Ordering},
    collections::{HashMap, HashSet},
    convert::Infallible,
    fmt::{self, Display},
};

use anyhow::Context;
use enum_map::{enum_map, Enum, EnumMap};
use itertools::process_results;
use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::eof,
    error::{ErrorKind, ParseError},
    Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
    tag::complete::tag, ParserExt,
};

use crate::{library::ITResult, parser};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum)]
enum Property {
    Extreme,
    Musical,
    Aerodynamic,
    Shiny,
}

fn parse_property_name(input: &str) -> ITResult<&str, Property> {
    use Property::*;

    alt((
        char('x').value(Extreme),
        char('m').value(Musical),
        char('a').value(Aerodynamic),
        char('s').value(Shiny),
    ))
    .parse(input)
}

#[derive(Debug, Clone, Copy, Default)]
struct Part {
    ratings: EnumMap<Property, i64>,
}

impl Part {
    fn rate(&self) -> i64 {
        self.ratings.values().copied().sum()
    }
}

impl Extend<(Property, i64)> for Part {
    fn extend<T: IntoIterator<Item = (Property, i64)>>(&mut self, iter: T) {
        iter.into_iter()
            .for_each(|(key, value)| self.ratings[key] = value)
    }
}

fn parse_property_pair(input: &str) -> ITResult<&str, (Property, i64)> {
    parser! {
        parse_property_name => key,
        char('='),
        digit1.parse_from_str_cut() => value;
        (key, value)
    }
    .parse(input)
}

fn parse_part(input: &str) -> ITResult<&str, Part> {
    collect_separated_terminated(parse_property_pair, char(','), char('}'))
        .preceded_by(char('{'))
        .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Accept,
    Reject,
}

fn parse_outcome(input: &str) -> ITResult<&str, Outcome> {
    use Outcome::*;

    alt((char('A').value(Accept), char('R').value(Reject))).parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BlockLabel<'a>(&'a str);

impl Display for BlockLabel<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

fn parse_block_label(input: &str) -> ITResult<&str, BlockLabel<'_>> {
    // Custom implementation because we want only lowercase
    let split_point = input
        .find(|c: char| !c.is_lowercase())
        .unwrap_or(input.len());

    let (label, tail) = input.split_at(split_point);

    match split_point {
        0 => Err(nom::Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::Alpha,
        ))),
        _point => Ok((tail, BlockLabel(label))),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Less,
    Greater,
}

impl Op {
    #[inline]
    #[must_use]
    fn evaluate<T: Ord>(self, lhs: &T, rhs: &T) -> bool {
        use Op::*;

        matches!(
            (Ord::cmp(lhs, rhs), self),
            (Ordering::Less, Less) | (Ordering::Greater, Greater)
        )
    }
}

fn parse_op(input: &str) -> ITResult<&str, Op> {
    use Op::*;

    alt((char('<').value(Less), char('>').value(Greater))).parse(input)
}

#[derive(Debug, Clone, Copy)]
struct Condition {
    property: Property,
    op: Op,
    value: i64,
}

impl Condition {
    #[inline]
    #[must_use]
    fn evaluate(&self, part: &Part) -> bool {
        self.op.evaluate(&part.ratings[self.property], &self.value)
    }
}

fn parse_condition(input: &str) -> ITResult<&str, Condition> {
    parser! {
        parse_property_name => property,
        parse_op => op,
        digit1.parse_from_str_cut() => value;
        Condition { property, op, value }
    }
    .parse(input)
}

#[derive(Debug, Clone, Copy)]
enum Jump<'a> {
    Outcome(Outcome),
    Block(BlockLabel<'a>),
}

fn parse_jump(input: &str) -> ITResult<&str, Jump<'_>> {
    alt((
        parse_outcome.map(Jump::Outcome),
        parse_block_label.map(Jump::Block),
    ))
    .parse(input)
}

fn parse_conditional_jump(input: &str) -> ITResult<&str, (Condition, Jump<'_>)> {
    parser! {
        parse_condition => condition,
        char(':'),
        parse_jump => jump;
        (condition, jump)
    }
    .parse(input)
}

#[derive(Debug, Clone)]
struct Block<'a> {
    conditions: Vec<(Condition, Jump<'a>)>,
    unconditional_jump: Jump<'a>,
}

impl<'a> Block<'a> {
    pub fn dependencies(&self) -> impl Iterator<Item = BlockLabel<'a>> + '_ {
        self.conditions
            .iter()
            .map(|(_, jump)| jump)
            .chain([&self.unconditional_jump])
            .filter_map(|jump| match *jump {
                Jump::Outcome(_) => None,
                Jump::Block(block) => Some(block),
            })
    }
}

fn parse_block(input: &str) -> ITResult<&str, Block<'_>> {
    // TODO: parse_separated_terminated needs to learn how to return the terminator
    let mut conditions = Vec::new();

    let (mut input, _) = char('{').parse(input)?;

    loop {
        input = match parse_conditional_jump.terminated(char(',')).parse(input) {
            Ok((tail, (condition, jump))) => {
                conditions.push((condition, jump));
                tail
            }
            Err(nom::Err::Error(err)) => {
                break match parse_jump.terminated(char('}')).parse(input) {
                    Ok((tail, unconditional_jump)) => Ok((
                        tail,
                        Block {
                            conditions,
                            unconditional_jump,
                        },
                    )),
                    Err(nom::Err::Error(err2)) => Err(nom::Err::Error(err.or(err2))),
                    Err(err) => Err(err),
                }
            }
            Err(err) => break Err(err),
        }
    }
}

#[derive(Debug, Clone)]
struct Hir<'a> {
    blocks: HashMap<BlockLabel<'a>, Block<'a>>,
}

impl Hir<'_> {
    fn evaluate(&self, part: &Part) -> anyhow::Result<Outcome> {
        let mut current_block = BlockLabel("in");
        let mut seen_blocks = HashSet::new();

        loop {
            if !seen_blocks.insert(current_block) {
                anyhow::bail!("infinite loop through {current_block}");
            }

            let block = self
                .blocks
                .get(&current_block)
                .with_context(|| format!("jump to nonexistent block {current_block}"))?;

            let jump = block
                .conditions
                .iter()
                .find(|(condition, _)| condition.evaluate(part))
                .map(|(_, jump)| jump)
                .unwrap_or(&block.unconditional_jump);

            match jump {
                &Jump::Outcome(outcome) => return Ok(outcome),
                &Jump::Block(block) => current_block = block,
            }
        }
    }
}

#[derive(Debug)]
pub struct Input<'a> {
    hir: Hir<'a>,
    parts: Vec<Part>,
}

fn parse_input(input: &str) -> ITResult<&str, Input<'_>> {
    parser! {
        collect_separated_terminated(
            parse_block_label.and(parse_block),
            char('\n'),
            tag("\n\n")
        ) => blocks,
        collect_separated_terminated(
            parse_part,
            char('\n'),
            multispace0.terminated(eof)
        ) => parts;
        Input{hir: Hir{blocks}, parts}
    }
    .parse(input)
}

impl<'a> TryFrom<&'a str> for Input<'a> {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input<'_>) -> anyhow::Result<i64> {
    let evaluations = input.parts.iter().map(|part| {
        input
            .hir
            .evaluate(part)
            .map(|outcome| (outcome, part.rate()))
    });

    process_results(evaluations, |evaluations| {
        evaluations
            .filter(|&(outcome, _)| outcome == Outcome::Accept)
            .map(|(_, rating)| rating)
            .sum()
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Bound {
    min: i64,
    max: i64,
}

impl Bound {
    pub fn new() -> Self {
        Self { min: 1, max: 4001 }
    }

    pub fn width(&self) -> i64 {
        self.max - self.min
    }

    pub fn apply(self, op: Op, bound: i64) -> Self {
        match op {
            Op::Less => Self {
                min: self.min,
                max: cmp::min(self.max, bound),
            },
            Op::Greater => Self {
                max: self.max,
                min: cmp::max(self.min, bound + 1),
            },
        }
    }

    pub fn reject(self, op: Op, bound: i64) -> Self {
        match op {
            Op::Less => self.apply(Op::Greater, bound - 1),
            Op::Greater => self.apply(Op::Less, bound + 1),
        }
    }

    pub fn merge(self, rhs: Self) -> Self {
        let candidate = Self {
            min: cmp::max(self.min, rhs.min),
            max: cmp::min(self.max, rhs.max),
        };

        if candidate.width() < 0 {
            Self { min: 0, max: 0 }
        } else {
            candidate
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PartConstraint {
    bounds: EnumMap<Property, Bound>,
}

impl PartConstraint {
    pub fn new() -> Self {
        Self {
            bounds: EnumMap::from_fn(|_| Bound::new()),
        }
    }

    pub fn width(&self) -> i64 {
        self.bounds.values().map(|bound| bound.width()).product()
    }

    pub fn apply(self, condition: Condition) -> Self {
        Self {
            bounds: EnumMap::from_fn(|property| {
                let bound = self.bounds[property];

                if property == condition.property {
                    bound.apply(condition.op, condition.value)
                } else {
                    bound
                }
            }),
        }
    }

    pub fn reject(self, condition: Condition) -> Self {
        Self {
            bounds: EnumMap::from_fn(|property| {
                let bound = self.bounds[property];

                if property == condition.property {
                    bound.reject(condition.op, condition.value)
                } else {
                    bound
                }
            }),
        }
    }

    pub fn merge(self, rhs: Self) -> Self {
        Self {
            bounds: enum_map! {
                property => self.bounds[property].merge(rhs.bounds[property])
            },
        }
    }
}

type ConstraintSet = HashSet<PartConstraint>;

pub fn part2(input: Input<'_>) -> anyhow::Result<i64> {
    let mut computed_constraint_sets: HashMap<BlockLabel<'_>, ConstraintSet> =
        HashMap::with_capacity(input.hir.blocks.len());

    loop {
        // Find a block that we haven't computed yet but for which we know
        // all dependencies. We assume that the chain of blocks forms a DAG.
        let (&label, block) = input
            .hir
            .blocks
            .iter()
            .filter(|(label, _block)| !computed_constraint_sets.contains_key(label))
            .find(|(_label, block)| {
                block
                    .dependencies()
                    .all(|dep| computed_constraint_sets.contains_key(&dep))
            })
            .context("Couldn't find an uncomputed block with all known dependencies")?;

        let mut base_constraint = PartConstraint::new();
        let mut constraint_set = ConstraintSet::new();

        for &(condition, ref jump) in &block.conditions {
            let local_constraint = base_constraint.apply(condition);

            match jump {
                Jump::Outcome(Outcome::Accept) => {
                    constraint_set.insert(local_constraint);
                }
                Jump::Outcome(Outcome::Reject) => {}
                Jump::Block(label) => constraint_set.extend(
                    computed_constraint_sets
                        .get(label)
                        .expect("confirmed that all dependencies exist")
                        .iter()
                        .map(|dependency_constraint| dependency_constraint.merge(local_constraint)),
                ),
            }

            base_constraint = base_constraint.reject(condition);
        }

        match block.unconditional_jump {
            Jump::Outcome(Outcome::Accept) => {
                constraint_set.insert(base_constraint);
            }
            Jump::Outcome(Outcome::Reject) => {}
            Jump::Block(ref label) => constraint_set.extend(
                computed_constraint_sets
                    .get(label)
                    .expect("confirmed that all dependencies exist")
                    .iter()
                    .map(|dependency_constraint| dependency_constraint.merge(base_constraint)),
            ),
        }

        if label == BlockLabel("in") {
            break Ok(constraint_set
                .iter()
                .map(|constraint| constraint.width())
                .sum());
        } else {
            computed_constraint_sets.insert(label, constraint_set);
        }
    }
}
