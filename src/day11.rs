use std::{
    cmp::{max, min},
    collections::{BTreeSet, HashSet},
    convert::Infallible,
};

use gridly::prelude::*;

use crate::library::IterExt;

#[derive(Debug)]
pub struct Input {
    galaxies: HashSet<Location>,
}

impl Input {
    fn bounds(&self) -> (RowRange, ColumnRange) {
        let mut galaxies = self.galaxies.iter().copied();

        let Some(root) = galaxies.next() else {
            return (Row(0).span(Rows(0)), Column(0).span(Columns(0)));
        };

        let (lower, upper) = galaxies.fold((root, root), |(lower, upper), galaxy| {
            (
                Location {
                    row: min(lower.row, galaxy.row),
                    column: min(lower.column, galaxy.column),
                },
                Location {
                    row: max(upper.row, galaxy.row),
                    column: max(upper.column, galaxy.column),
                },
            )
        });

        (
            lower.row.range_to(upper.row + Rows(1)),
            lower.column.range_to(upper.column + Columns(1)),
        )
    }
}

impl TryFrom<&str> for Input {
    type Error = Infallible;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self {
            galaxies: value
                .lines()
                .with_rows(Row(0))
                .flat_map(|(row, content)| {
                    content
                        .as_bytes()
                        .iter()
                        .with_columns(Column(0))
                        .filter(|&(_column, &byte)| byte == b'#')
                        .map(move |(column, _byte)| row + column)
                })
                .collect(),
        })
    }
}

fn compute(input: &Input, empty_space_factor: isize) -> isize {
    let (row_range, column_range) = input.bounds();
    let factor = empty_space_factor - 1;

    let empty_rows: BTreeSet<Row> = row_range
        .filter(|&row| input.galaxies.iter().all(|galaxy| galaxy.row != row))
        .collect();

    let empty_columns: BTreeSet<Column> = column_range
        .filter(|&column| input.galaxies.iter().all(|galaxy| galaxy.column != column))
        .collect();

    let updated_galaxies: Vec<Location> = input
        .galaxies
        .iter()
        .map(|&galaxy| {
            let empty_rows_count = empty_rows.range(..galaxy.row).count();
            let empty_columns_count = empty_columns.range(..galaxy.column).count();

            galaxy
                + Rows(empty_rows_count as isize * factor)
                + Columns(empty_columns_count as isize * factor)
        })
        .collect();

    updated_galaxies
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, galaxy)| {
            updated_galaxies
                .get(i + 1..)
                .unwrap_or(&[])
                .iter()
                .copied()
                .map(move |galaxy2| (galaxy, galaxy2))
        })
        .map(|(galaxy, galaxy2)| (galaxy - galaxy2).manhattan_length())
        .sum()
}

pub fn part1(input: Input) -> anyhow::Result<isize> {
    Ok(compute(&input, 2))
}

pub fn part2(input: Input) -> anyhow::Result<isize> {
    Ok(compute(&input, 1_000_000))
}
