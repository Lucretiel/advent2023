include!(concat!(env!("OUT_DIR"), "/generated.rs"));

mod library;

use std::{
    fs::File,
    io::{self, Read},
    num::ParseIntError,
    path::PathBuf,
    str::FromStr,
};

use anyhow::Context;
use clap::Parser;
use lazy_format::lazy_format;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum DayError {
    #[error("Failed to parse day")]
    Parse(#[from] ParseIntError),

    #[error("{0} is not an Advent Puzzle Day")]
    BadDay(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Part {
    Part1,
    Part2,
}

#[derive(Debug, Clone, Error)]
pub enum PartError {
    #[error("Failed to parse part")]
    Parse(#[from] ParseIntError),

    #[error("{0} is not an Advent Puzzle Part; must be 1 or 2")]
    BadPart(u8),
}

impl FromStr for Part {
    type Err = PartError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value: u8 = s.parse()?;

        match value {
            1 => Ok(Part::Part1),
            2 => Ok(Part::Part2),
            value => Err(PartError::BadPart(value)),
        }
    }
}

/// Solve an Advent of Code 2022 problem for the given day and part. Unless
/// --string or --file are given, input is read from standard input. The
/// solution is always written to standard output.
#[derive(Parser)]
#[command(group(clap::ArgGroup::new("input")))]
struct Args {
    /// The advent of code day to solve
    #[arg(short, long)]
    day: Day,

    /// Which part of the day to solve
    #[arg(short, long)]
    part: Part,

    #[arg(short = 'v', long)]
    show_input: bool,

    /// If given, read input from this file
    #[arg(short, long, group = "input")]
    file: Option<PathBuf>,

    /// If given, use this as the puzzle input directly
    #[arg(short, long, group = "input")]
    string: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args: Args = Args::parse();

    let buf = match args.string {
        Some(buf) => buf,
        None => {
            let mut buf = String::new();
            match args.file {
                Some(file) => File::open(&file)
                    .context(lazy_format!("failed to open file: {:?}", file.display()))?
                    .read_to_string(&mut buf)
                    .context("failed to read puzzle input from file")?,
                None => io::stdin()
                    .read_to_string(&mut buf)
                    .context("failed to read puzzle input from stdin")?,
            };
            buf
        }
    };

    run_solution(args.day, args.part, &buf, args.show_input)
}
