use anyhow::Context;
use itertools::{process_results, Itertools};
use lazy_format::lazy_format;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[derive(Debug)]
struct Race {
    distance: i64,
    duration: i64,
}

impl Race {
    pub fn test(&self, speed: i64) -> bool {
        speed * (self.duration - speed) > self.distance
    }
    // Find the inclusive minimum and maximum values for which we beat the
    // race time
    pub fn solutions(&self) -> Option<(i64, i64)> {
        /*
        find all speeds for which:
        speed * (time - speed) > old_distance
        speed * time - speed**2 > old_distance
        - (speed**2) + speed * time - old_distance > 0

        Solve for speed, we get:

        speed = 1/2 ( sqrt (t**2 - 4d) +- t )
        */

        /*
        Or we can just brute force it
        */

        let min = (1..self.duration).find(|&speed| self.test(speed))?;
        let max = (1..self.duration).rfind(|&speed| self.test(speed))?;

        Some((min, max))
    }
}

#[derive(Debug)]
pub struct Input {
    races: Vec<Race>,
}

fn parse_prefixed_number_list<'a>(
    prefix: &'static str,
    line: &'a str,
) -> anyhow::Result<impl Iterator<Item = anyhow::Result<i64>> + 'a> {
    line.strip_prefix(prefix)
        .context(lazy_format!("line didn't have the prefix {prefix:?}"))
        .map(move |line| {
            line.split_whitespace().map(|token| {
                token
                    .parse()
                    .context(lazy_format!("failed to parse integer"))
            })
        })
}

impl TryFrom<&str> for Input {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut lines = value.trim().lines();

        let duration_line = lines.next().context("Input didn't have a duration line")?;
        let distance_line = lines
            .exactly_one()
            .ok()
            .context("Input didn't have a distance line, or it had too many lines")?;

        let durations = parse_prefixed_number_list("Time:", duration_line)?;
        let distances = parse_prefixed_number_list("Distance:", distance_line)?;

        let pairs = durations.zip_longest(distances);

        use itertools::EitherOrBoth::*;

        pairs
            .map(|pair| match pair {
                Both(duration, distance) => {
                    let duration = duration.context("error parsing duration value")?;
                    let distance = distance.context("error parsing distance value")?;

                    Ok(Race { distance, duration })
                }
                Left(_) | Right(_) => Err(anyhow::anyhow!(
                    "Duration Line and Distance Line were different lengths"
                )),
            })
            .try_collect()
            .map(|races| Input { races })
    }
}

pub fn part1(input: Input) -> anyhow::Result<i64> {
    let solutions = input.races.iter().enumerate().map(|(i, race)| {
        race.solutions()
            .context(lazy_format!("no possible ways to win race {i} "))
            .map(|(min, max)| max - min + 1)
    });

    process_results(solutions, |solutions| solutions.product())
}

#[derive(Debug)]
pub struct Input2 {
    race: Race,
}

fn parse_spacey_number(input: &str) -> i64 {
    input
        .bytes()
        .filter(|&b| b.is_ascii_digit())
        .map(|b| b - b'0')
        .map(|b| b as i64)
        .fold(0, |accum, d| accum * 10 + d)
}

impl TryFrom<&str> for Input2 {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut lines = value.trim().lines();

        let duration =
            parse_spacey_number(lines.next().context("input didn't have a duration line")?);

        let distance = parse_spacey_number(
            lines
                .exactly_one()
                .ok()
                .context("input didn't have a duration line, or it had too many lines")?,
        );

        Ok(Input2 {
            race: Race { duration, distance },
        })
    }
}

pub fn part2(input: Input2) -> anyhow::Result<i64> {
    input
        .race
        .solutions()
        .map(|(min, max)| max - min + 1)
        .context("no possible ways to win the race")
}
