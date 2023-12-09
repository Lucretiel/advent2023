use std::convert::Infallible;

use nom_supreme::{error::ErrorTree, final_parser::final_parser};

use crate::library::ITResult;

#[derive(Debug)]
pub struct Input<'a> {
    raw: &'a str,
}

fn parse_input(input: &str) -> ITResult<&str, Input<'_>> {
    Ok(("", Input { raw: input }))
}

impl<'a> TryFrom<&'a str> for Input<'a> {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input<'_>) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}

pub fn part2(input: Input<'_>) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}
