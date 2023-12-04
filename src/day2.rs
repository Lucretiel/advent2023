use std::{cmp::max, collections::HashMap};

use enum_map::{enum_map, Enum, EnumMap};
use nom::{
    branch::alt,
    character::complete::{digit1, space1},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, final_parser::final_parser, tag::complete::tag, ParserExt};

use crate::{
    library::{split_parser, split_parser_fold, Definitely},
    parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GameId(i32);

/// Parse something resembling "Game 3"
fn parse_game_id(input: &str) -> IResult<&str, GameId, ErrorTree<&str>> {
    digit1
        .parse_from_str_cut()
        .map(GameId)
        .preceded_by(tag("Game "))
        .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Enum)]
enum BlockColor {
    Red,
    Green,
    Blue,
}

/// Parse something resembling "green"
fn parse_color(input: &str) -> IResult<&str, BlockColor, ErrorTree<&str>> {
    use BlockColor::*;

    alt((
        tag("red").value(Red),
        tag("green").value(Green),
        tag("blue").value(Blue),
    ))
    .parse(input)
}

/// Parse something resembling "5 green"
fn parse_block_count(input: &str) -> IResult<&str, (u32, BlockColor), ErrorTree<&str>> {
    parser! {
        digit1.parse_from_str_cut() => count,
        space1,
        parse_color => color;
        (count, color)
    }
    .parse(input)
}

type Sample = EnumMap<BlockColor, u32>;

/// Parse something resembling "3 red, 4 blue"
fn parse_sample(input: &str) -> IResult<&str, Sample, ErrorTree<&str>> {
    split_parser_fold(
        parse_block_count,
        ", ",
        EnumMap::default,
        |mut map, (count, color)| {
            map[color] += count;
            map
        },
    )
    .parse(input)
}

type SampleList = Vec<Sample>;

fn parse_sample_list(input: &str) -> IResult<&str, SampleList, ErrorTree<&str>> {
    split_parser(parse_sample, "; ").parse(input)
}

fn parse_game_row(input: &str) -> IResult<&str, (GameId, SampleList), ErrorTree<&str>> {
    parser! {
        parse_game_id => game_id,
        tag(": "),
        parse_sample_list => samples;
        (game_id, samples)
    }
    .parse(input)
}

fn parse_input(input: &str) -> IResult<&str, Input, ErrorTree<&str>> {
    split_parser(parse_game_row, "\n")
        .map(|games| Input { games })
        .parse(input)
}

#[derive(Debug)]
pub struct Input {
    games: HashMap<GameId, SampleList>,
}

impl<'a> TryFrom<&'a str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> Definitely<i32> {
    let candidate_set = enum_map! {
        BlockColor::Red => 12u32,
        BlockColor::Green => 13,
        BlockColor::Blue => 14,
    };

    Ok(input
        .games
        .iter()
        .filter(|(_id, samples)| {
            samples.iter().all(|sample| {
                sample
                    .iter()
                    .all(|(color, &count)| count <= candidate_set[color])
            })
        })
        .map(|(&GameId(id), _samples)| id)
        .sum())
}

pub fn part2(input: Input) -> Definitely<u32> {
    Ok(input
        .games
        .values()
        .map(|samples| -> u32 {
            let bag = samples.iter().fold(EnumMap::default(), |bag, sample| {
                enum_map! { color => max(bag[color], sample[color]) }
            });

            // Power is the product of the counts
            bag.values().product()
        })
        .sum())
}
