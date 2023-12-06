use std::collections::{BTreeMap, HashSet};

use nom::{
    branch::alt,
    character::complete::{char, digit1, space0, space1},
    combinator::{eof, success},
    IResult, Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
    tag::complete::tag, ParserExt,
};

use crate::{
    library::{split_parser, Counter, Definitely},
    parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct CardId(i64);

fn parse_card_id(input: &str) -> IResult<&str, CardId, ErrorTree<&str>> {
    tag("Card")
        .terminated(space1)
        .precedes(digit1)
        .parse_from_str_cut()
        .map(CardId)
        .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Value(i64);

fn parse_value(input: &str) -> IResult<&str, Value, ErrorTree<&str>> {
    digit1.parse_from_str_cut().map(Value).parse(input)
}

fn parse_value_list<T>(input: &str) -> IResult<&str, T, ErrorTree<&str>>
where
    T: Extend<Value> + Default,
{
    collect_separated_terminated(
        parse_value.terminated(space0),
        success(()),
        alt((char('|'), char('\n'), eof.value('.'))).peek(),
    )
    .parse(input)
}

#[derive(Debug)]
struct CardContents {
    winning_values: HashSet<Value>,
    given_values: HashSet<Value>,
}

impl CardContents {
    fn count_matches(&self) -> i64 {
        self.winning_values.intersection(&self.given_values).count() as i64
    }
}

fn parse_card(input: &str) -> IResult<&str, (CardId, CardContents), ErrorTree<&str>> {
    parser! {
        parse_card_id.context("card id") => card_id,
        char(':'),
        space0,
        parse_value_list.context("winning values") => winning_values,
        char('|'),
        space0,
        parse_value_list.context("given values") => given_values;
        (card_id, CardContents{winning_values, given_values})
    }
    .parse(input)
}

fn parse_card_list<T>(input: &str) -> IResult<&str, T, ErrorTree<&str>>
where
    T: Extend<(CardId, CardContents)> + Default + 'static,
{
    split_parser(parse_card, "\n").parse(input)
}

fn parse_input(input: &str) -> IResult<&str, Input, ErrorTree<&str>> {
    parse_card_list.map(|cards| Input { cards }).parse(input)
}

#[derive(Debug)]
pub struct Input {
    cards: BTreeMap<CardId, CardContents>,
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> Definitely<i64> {
    Ok(input
        .cards
        .values()
        .map(|card| match card.count_matches().checked_sub(1) {
            None => 0,
            Some(doublings) => 2i64.pow(doublings as u32),
        })
        .sum())
}

pub fn part2(input: Input) -> Definitely<usize> {
    let mut card_counts: Counter<CardId> = input.cards.keys().copied().collect();

    for (&card_id, card) in &input.cards {
        let matches = card.count_matches();

        let new_card_start = card_id.0 + 1;
        let new_card_end = new_card_start + matches;

        let count = card_counts.get(&card_id);

        eprintln!("adding {count}x {new_card_start}..{new_card_end}");

        card_counts.extend(
            (new_card_start..new_card_end)
                .map(CardId)
                .map(|new_card_id| (new_card_id, count)),
        );
    }

    Ok(card_counts.iter().map(|(_, count)| count).sum())
}
