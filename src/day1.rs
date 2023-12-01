use anyhow::Context;
use lazy_format::lazy_format;

pub fn part1(input: &str) -> anyhow::Result<u32> {
    // Iterator of results
    let values = input.lines().zip(1..).map(|(line, line_number)| {
        let line = line.trim();

        let first = line
            .chars()
            .find(|c| c.is_ascii_digit())
            .context(lazy_format!("no digits in line {line_number}"))?
            .to_digit(10)
            .expect("filtered for an ascii digit");

        let last = line
            .chars()
            .rfind(|c| c.is_ascii_digit())
            .context(lazy_format!("no digits in line {line_number}"))?
            .to_digit(10)
            .expect("filtered for an ascii digit");

        Ok((first * 10) + last)
    });

    itertools::process_results(values, |values| values.sum())
}

struct StringTails<'a> {
    string: &'a str,
}

impl<'a> Iterator for StringTails<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.string.chars();
        let _ = chars.next()?;
        let item = self.string;
        self.string = chars.as_str();
        Some(item)
    }
}

fn match_prefix_digit(string: &str) -> Option<u32> {
    [
        ["zero", "0"],
        ["one", "1"],
        ["two", "2"],
        ["three", "3"],
        ["four", "4"],
        ["five", "5"],
        ["six", "6"],
        ["seven", "7"],
        ["eight", "8"],
        ["nine", "9"],
    ]
    .iter()
    .position(|digit_set| digit_set.iter().any(|digit| string.starts_with(digit)))
    .map(|idx| idx as u32)
}

fn digit_stream(s: &str) -> impl Iterator<Item = u32> + DoubleEndedIterator + '_ {
    (0..s.len())
        .filter_map(|idx| s.get(idx..))
        .filter_map(|substring| match_prefix_digit(substring))
}

pub fn part2(input: &str) -> anyhow::Result<u32> {
    let values = input.lines().enumerate().map(|(i, line)| {
        let first = digit_stream(line)
            .next()
            .context(lazy_format!("no digit in line index {i}"))?;

        let last = digit_stream(line)
            .next_back()
            .context(lazy_format!("no digit in line index {i}"))?;

        Ok((first * 10) + last)
    });

    itertools::process_results(values, |values| values.sum())
}
