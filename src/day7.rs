use enum_map::{Enum, EnumMap};
use nom::character::complete::{digit1, multispace0, space1};
use nom::combinator::eof;
use nom::{branch::alt, character::complete::char};
use nom::{IResult, Parser};
use nom_supreme::multi::collect_separated_terminated;
use nom_supreme::ParserExt;
use nom_supreme::{error::ErrorTree, final_parser::final_parser};

use crate::library::{EnumCounter, ITResult};
use crate::parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Enum)]
#[repr(u8)]
enum Card {
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
    Ten = 10,
    Jack = 11,
    Queen = 12,
    King = 13,
    Ace = 14,
}

impl Card {
    /// Compute a sort key using joker order, where jokers sort less than all
    /// other cards
    #[inline]
    #[must_use]
    fn joker_key(self) -> (u8, Self) {
        match self {
            Card::Jack => (0, Card::Jack),
            card => (1, card),
        }
    }
}

fn parse_card(input: &str) -> ITResult<&str, Card> {
    use Card::*;

    alt((
        char('2').value(Two),
        char('3').value(Three),
        char('4').value(Four),
        char('5').value(Five),
        char('6').value(Six),
        char('7').value(Seven),
        char('8').value(Eight),
        char('9').value(Nine),
        char('T').value(Ten),
        char('J').value(Jack),
        char('Q').value(Queen),
        char('K').value(King),
        char('A').value(Ace),
    ))
    .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Category {
    HighCard,
    Pair,
    TwoPair,
    Triple,
    FullHouse,
    Quad,
    Quint,
}

#[derive(Debug, Copy, Clone)]
struct Hand {
    cards: [Card; 5],
}

impl Hand {
    fn evaluate(&self) -> Category {
        // Compute, for each card, how many of that card are present
        let counts: EnumCounter<Card> = self.cards.iter().copied().collect();

        // Compute, for each *count*, how many of that count are present.
        let count_counts: [u8; 5] =
            counts
                .iter()
                .map(|(_card, count)| count)
                .fold([0; 5], |mut map, count| {
                    map[(count - 1) as usize] += 1;
                    map
                });

        match count_counts {
            [_, _, _, _, 1] => Category::Quint,
            [_, _, _, 1, _] => Category::Quad,
            [_, 1, 1, _, _] => Category::FullHouse,
            [_, _, 1, _, _] => Category::Triple,
            [_, 2, _, _, _] => Category::TwoPair,
            [_, 1, _, _, _] => Category::Pair,
            _ => Category::HighCard,
        }
    }

    fn evaluate_joker(&self) -> Category {
        // Compute, for each card (besides jokers), how many of that card are present
        let counts: EnumMap<Card, u8> = self
            .cards
            .iter()
            .copied()
            .filter(|&card| card != Card::Jack)
            .fold(EnumMap::default(), |mut map, card| {
                map[card] += 1;
                map
            });

        // Identify the card that there are the most of
        let (best_card, _) = counts
            .iter()
            .rev()
            .max_by_key(|(_card, &count)| count)
            .expect("there are definitely at least 1 card");

        // Transform all jokers into that card
        let modified_hand = Hand {
            cards: self.cards.map(|card| match card {
                Card::Jack => best_card,
                card => card,
            }),
        };

        modified_hand.evaluate()
    }
}

fn parse_hand(input: &str) -> ITResult<&str, Hand> {
    parse_card.array().map(|cards| Hand { cards }).parse(input)
}

#[derive(Debug, Clone, Copy)]
struct Play {
    hand: Hand,
    bid: i32,
}

fn parse_play(input: &str) -> ITResult<&str, Play> {
    parser! {
        parse_hand => hand,
        space1,
        digit1.parse_from_str_cut() => bid;
        Play {hand, bid}
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    plays: Vec<Play>,
}

fn parse_input(input: &str) -> IResult<&str, Input, ErrorTree<&str>> {
    collect_separated_terminated(parse_play, char('\n'), multispace0.terminated(eof))
        .map(|plays| Input { plays })
        .parse(input)
}
impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(Input { mut plays }: Input) -> anyhow::Result<i32> {
    plays.sort_unstable_by_key(|play| {
        let category = play.hand.evaluate();
        (category, play.hand.cards)
    });

    Ok(plays
        .iter()
        .zip(1..)
        .map(|(play, rank)| play.bid * rank)
        .sum())
}

pub fn part2(Input { mut plays }: Input) -> anyhow::Result<i32> {
    plays.sort_unstable_by_key(|play| {
        let category = play.hand.evaluate_joker();
        // Convert the hand of cards into a hand of (value, card) tuples.
        // This makes it easy to sort jokers less than all other cards.
        (category, play.hand.cards.map(|card| card.joker_key()))
    });

    Ok(plays
        .iter()
        .zip(1..)
        .map(|(play, rank)| play.bid * rank)
        .sum())
}
