use std::collections::BTreeMap;

use anyhow::Context;
use nom::{
    character::complete::{alpha1, digit1, multispace0, space1},
    combinator::{eof, success},
    Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
    tag::complete::tag, ParserExt,
};
use rayon::prelude::*;

use crate::{library::ITResult, parser};

#[derive(Debug, Copy, Clone)]
struct MappingRange {
    input_base: i64,
    output_base: i64,
    width: i64,
}

impl MappingRange {
    /// Project an input to an output, without regard for the width
    #[inline]
    pub fn project(&self, input: i64) -> i64 {
        self.output_base + (input - self.input_base)
    }
}

#[derive(Debug, Copy, Clone)]
struct MappingOut {
    base: i64,
    width: i64,
}

#[derive(Debug, Clone, Default)]
struct Mapping {
    ranges: BTreeMap<i64, MappingOut>,
}

impl Mapping {
    pub fn add(&mut self, range: MappingRange) {
        // TODO: check for overlaps?
        self.ranges.insert(
            range.input_base,
            MappingOut {
                base: range.output_base,
                width: range.width,
            },
        );
    }

    pub fn get_matching_range(&self, input: i64) -> Option<MappingRange> {
        self.ranges
            .range(..=input)
            .next_back()
            .map(|(&key, out)| MappingRange {
                input_base: key,
                output_base: out.base,
                width: out.width,
            })
            .filter(|range| input < range.input_base + range.width)
    }

    pub fn compute(&self, input: i64) -> i64 {
        self.get_matching_range(input)
            .map(|range| range.project(input))
            .unwrap_or(input)
    }
}

impl Extend<MappingRange> for Mapping {
    fn extend<T: IntoIterator<Item = MappingRange>>(&mut self, iter: T) {
        iter.into_iter().for_each(|range| self.add(range))
    }
}

fn parse_mapping_range(input: &str) -> ITResult<&str, MappingRange> {
    digit1
        .parse_from_str_cut()
        .separated_array(space1)
        .map(|[output_base, input_base, width]| MappingRange {
            input_base,
            output_base,
            width,
        })
        .parse(input)
}

fn parse_mapping(input: &str) -> ITResult<&str, Mapping> {
    collect_separated_terminated(
        parse_mapping_range,
        tag("\n"),
        tag("\n\n").or(multispace0.terminated(eof)),
    )
    .parse(input)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Category<'i>(&'i str);

fn parse_category(input: &str) -> ITResult<&str, Category<'_>> {
    alpha1.map(Category).parse(input)
}

#[derive(Debug)]
struct MappingRule<'a> {
    input: Category<'a>,
    output: Category<'a>,
    mapping: Mapping,
}

fn parse_mapping_rule(input: &str) -> ITResult<&str, MappingRule<'_>> {
    parser! {
        parse_category => input,
        tag("-to-"),
        parse_category => output,
        tag(" map:\n"),
        parse_mapping => mapping;
        MappingRule{input, output, mapping}
    }
    .parse(input)
}

#[derive(Debug)]
struct MappingRules<'a> {
    rules: Vec<MappingRule<'a>>,
}

impl<'a> MappingRules<'a> {
    pub fn compute(&self, origin: Category<'_>, value: i64) -> Option<(Category<'a>, i64)> {
        self.rules
            .iter()
            .find(|rule| rule.input == origin)
            .map(|rule| (rule.output, rule.mapping.compute(value)))
    }

    pub fn seed_to_location_number(&self, mut value: i64) -> Option<i64> {
        // TODO: loop detection
        let mut category = Category("seed");

        while category != Category("location") {
            let (new_category, new_value) = self.compute(category, value)?;
            category = new_category;
            value = new_value;
        }

        Some(value)
    }
}

fn parse_mapping_rules(input: &str) -> ITResult<&str, MappingRules<'_>> {
    collect_separated_terminated(parse_mapping_rule, success(()), eof)
        .map(|rules| MappingRules { rules })
        .parse(input)
}

fn parse_input(input: &str) -> ITResult<&str, Input<'_>> {
    parser! {
        tag("seeds: "),
        collect_separated_terminated(
            digit1.parse_from_str_cut::<i64>(),
            tag(" "),
            tag("\n\n"),
        ) => seeds,
        parse_mapping_rules => rules;
        Input{seeds, rules}
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input<'a> {
    seeds: Vec<i64>,
    rules: MappingRules<'a>,
}

impl<'a> TryFrom<&'a str> for Input<'a> {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input<'_>) -> anyhow::Result<i64> {
    input
        .seeds
        .iter()
        .filter_map(|&seed| input.rules.seed_to_location_number(seed))
        .min()
        .context("no seeds had a location number")
}

pub fn part2(input: Input<'_>) -> anyhow::Result<i64> {
    input
        .seeds
        .par_chunks_exact(2)
        .flat_map(|pair| (pair[0]..pair[0] + pair[1]))
        .filter_map(|seed| input.rules.seed_to_location_number(seed))
        .min()
        .context("no seeds had a location number")
}
