use itertools::Itertools;
use nom::{
    character::complete::{char, digit1, space1},
    combinator::{eof, success},
    Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
    tag::complete::tag, ParserExt,
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::library::{ITResult, IterExt};

#[derive(Debug)]
struct Sample {
    data: Vec<i64>,
}

impl Sample {
    pub fn extrapolate_end(&self) -> i64 {
        extrapolate_end(&self.data)
    }

    pub fn extrapolate_front(&self) -> i64 {
        extrapolate_front(&self.data)
    }
}

fn extrapolate_end(data: &[i64]) -> i64 {
    match *data {
        [] => 0,
        [head, ref tail @ ..] if tail.iter().all(|&value| value == head) => head,
        [.., last] => {
            let deltas = data
                .iter()
                .copied()
                .streaming_windows()
                .map(|[a, b]| b - a)
                .collect_vec();

            let tail_delta = extrapolate_end(&deltas);
            last + tail_delta
        }
    }
}

fn extrapolate_front(data: &[i64]) -> i64 {
    match *data {
        [] => 0,
        [head, ref tail @ ..] if tail.iter().all(|&value| value == head) => head,
        [head, ..] => {
            let deltas = data
                .iter()
                .copied()
                .streaming_windows()
                .map(|[a, b]| b - a)
                .collect_vec();

            let head_delta = extrapolate_front(&deltas);
            head - head_delta
        }
    }
}

fn parse_sample(input: &str) -> ITResult<&str, Sample> {
    collect_separated_terminated(
        digit1
            .opt_preceded_by(char('-'))
            .recognize()
            .parse_from_str_cut::<i64>(),
        space1,
        tag("\n").or(eof),
    )
    .map(|data| Sample { data })
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    samples: Vec<Sample>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_sample, success(()), eof)
        .map(|samples| Input { samples })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> anyhow::Result<i64> {
    Ok(input
        .samples
        .par_iter()
        .map(|sample| sample.extrapolate_end())
        .sum())
}

pub fn part2(input: Input) -> anyhow::Result<i64> {
    Ok(input
        .samples
        .par_iter()
        .map(|sample| sample.extrapolate_front())
        .sum())
}
