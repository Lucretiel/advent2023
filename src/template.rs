use std::convert::Infallible;

use nom_supreme::{error::ErrorTree, final_parser::final_parser};

use crate::library::ITResult;

#[derive(Debug)]
pub struct Input {
    raw: String,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    Ok(("", Input { raw: input }))
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}

pub fn part2(input: Input) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}
