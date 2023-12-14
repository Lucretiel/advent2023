use std::{collections::HashSet, ops::Sub};

use anyhow::Context;
use gridly::prelude::*;
use gridly_grids::VecGrid;
use itertools::Itertools;
use rayon::prelude::*;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum Cell {
    #[default]
    Ash,
    Rock,
}

#[derive(Debug, Clone)]
struct Map {
    grid: VecGrid<Cell>,
}

struct SmudgedMap<'a> {
    grid: &'a VecGrid<Cell>,
    smudge: Location,
}

impl GridBounds for SmudgedMap<'_> {
    fn dimensions(&self) -> Vector {
        self.grid.dimensions()
    }

    fn root(&self) -> Location {
        self.grid.root()
    }
}

impl Grid for SmudgedMap<'_> {
    type Item = Cell;

    unsafe fn get_unchecked(&self, location: Location) -> &Self::Item {
        let cell = self.grid.get_unchecked(location);

        if location == self.smudge {
            match *cell {
                Cell::Ash => &Cell::Rock,
                Cell::Rock => &Cell::Ash,
            }
        } else {
            cell
        }
    }
}

#[derive(Debug)]
pub struct Input {
    maps: Vec<Map>,
}

impl TryFrom<&str> for Input {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value
            .split("\n\n")
            .map(|map| {
                VecGrid::new_from_rows(map.lines().map(|line| {
                    line.bytes().map(|b| match b {
                        b'#' => Cell::Rock,
                        _ => Cell::Ash,
                    })
                }))
                .context("failed to create map")
                .map(|grid| Map { grid })
            })
            .try_collect()
            .map(|maps| Input { maps })
    }
}

fn discover_reflection_lines<'a, C>(
    grid: &'a impl Grid<Item = Cell>,
) -> impl Iterator<Item = C> + 'a
where
    C: LocationComponent + 'a,
    C::Distance: Sub<C::Distance, Output = C::Distance>,
{
    let range = C::from(1).span(grid.dimension::<C::Distance>() - C::Distance::from(1));

    // Find all the indices that act as mirrors
    range.filter(|index| {
        // An index is a mirror if all of its cross indexes are mirrors
        grid.range().all(|cross_index| {
            let root = index.combine(cross_index);
            let deltas = (0..).map(C::Distance::from);

            deltas
                .map(|delta| (root - delta - C::Distance::from(1), root + delta))
                .map(|(a, b)| {
                    let a = grid.get(a).ok()?;
                    let b = grid.get(b).ok()?;
                    Some((a, b))
                })
                .while_some()
                .all(|(a, b)| a == b)
        })
    })
}

fn smudged_grids(base_grid: &Map) -> impl ParallelIterator<Item = SmudgedMap<'_>> {
    let root = base_grid.grid.root();
    let bound = base_grid.grid.outer_bound();

    let rows = root.row.0..bound.row.0;
    let columns = root.column.0..bound.column.0;

    rows.into_par_iter()
        .map(Row)
        .flat_map(move |row| {
            columns
                .clone()
                .into_par_iter()
                .map(Column)
                .map(move |column| row.combine(column))
        })
        .map(|location| SmudgedMap {
            grid: &base_grid.grid,
            smudge: location,
        })
}

fn score_map(map: &impl Grid<Item = Cell>) -> isize {
    let rows = discover_reflection_lines::<Row>(&map);
    let columns = discover_reflection_lines::<Column>(&map);

    let row_sum: isize = rows.map(|row| row.value()).sum();
    let column_sum: isize = columns.map(|column| column.value()).sum();

    row_sum * 100 + column_sum
}

pub fn part1(input: Input) -> anyhow::Result<isize> {
    Ok(input
        .maps
        .par_iter()
        .map(|map| &map.grid)
        .map(score_map)
        .sum())
}

pub fn part2(input: Input) -> anyhow::Result<isize> {
    Ok(input
        .maps
        .par_iter()
        .map(|map| {
            let base_rows: HashSet<Row> = discover_reflection_lines::<Row>(&map.grid).collect();
            let base_columns: HashSet<Column> =
                discover_reflection_lines::<Column>(&map.grid).collect();

            smudged_grids(map)
                .map(|smudged| {
                    let new_rows = discover_reflection_lines::<Row>(&smudged)
                        .filter(|row| !base_rows.contains(row));

                    let new_columns = discover_reflection_lines::<Column>(&smudged)
                        .filter(|column| !base_columns.contains(column));

                    let row_sum: isize = new_rows.map(|row| row.value()).sum();
                    let column_sum: isize = new_columns.map(|column| column.value()).sum();

                    row_sum * 100 + column_sum
                })
                .find_any(|&sum| sum != 0)
                .expect("there needs to be a valid smudge")
        })
        .sum())
}
