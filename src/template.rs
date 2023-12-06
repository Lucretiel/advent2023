use std::convert::Infallible;

use nom::IResult;
use nom_supreme::{error::ErrorTree, final_parser::final_parser};

fn parse_input(input: &str) -> IResult<&str, Input<'_>, ErrorTree<&str>> {
    Ok(("", Input { raw: input }))
}

#[derive(Debug)]
pub struct Input<'a> {
    raw: &'a str,
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
