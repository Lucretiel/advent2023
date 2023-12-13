use core::slice;
use std::{collections::HashMap, convert::Infallible, iter};

use joinery::{JoinItem, JoinableIterator};
use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0, multispace1, space1},
    combinator::{eof, success},
    Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated, ParserExt,
};
use rayon::prelude::*;

use crate::{
    library::{
        dynamic::{execute, Subtask, Task, TaskInterrupt},
        ITResult,
    },
    parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Cell {
    Empty,
    Filled,
    Unknown,
}

fn parse_cell(input: &str) -> ITResult<&str, Cell> {
    alt((
        char('.').value(Cell::Empty),
        char('#').value(Cell::Filled),
        char('?').value(Cell::Unknown),
    ))
    .parse(input)
}

#[derive(Debug, Clone)]
struct Row {
    cells: Vec<Cell>,
    clues: Vec<usize>,
}

impl Row {
    // To aid with computation, we insert an empty cell at the end of the row.
    // Blocks check their border conditions at the end, so it makes it easier
    fn normalize(mut self) -> Row {
        self.cells.push(Cell::Empty);

        self
    }

    fn unfold(self) -> Row {
        Row {
            cells: iter::repeat(self.cells.as_slice())
                .take(5)
                .iter_join_with(&Cell::Unknown)
                .flat_map(|cell| match cell {
                    JoinItem::Element(cells) => cells,
                    JoinItem::Separator(fold_point) => slice::from_ref(fold_point),
                })
                .copied()
                .collect(),

            clues: iter::repeat(self.clues.as_slice())
                .take(5)
                .flatten()
                .copied()
                .collect(),
        }
    }
}

fn block_fits_here(cells: &[Cell]) -> bool {
    match *cells {
        [ref block @ .., Cell::Empty | Cell::Unknown] => block
            .iter()
            .all(|&cell| matches!(cell, Cell::Unknown | Cell::Filled)),
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RowState<'a> {
    cells: &'a [Cell],
    clues: &'a [usize],
}

struct TaskState<'a> {
    clue: usize,
    sub_clues: &'a [usize],
    matching_block_positions: Vec<usize>,
}

struct CountPossibleArrangements;

impl<'a> Task<RowState<'a>, usize, Infallible> for CountPossibleArrangements {
    type State = TaskState<'a>;

    fn solve<'sub, T>(
        &self,
        goal: &RowState<'a>,
        subtasker: &'sub T,
        state: &mut Option<TaskState<'a>>,
    ) -> Result<usize, TaskInterrupt<'sub, RowState<'a>, Infallible>>
    where
        T: Subtask<RowState<'a>, usize>,
        RowState<'a>: 'sub,
    {
        let state = match state.as_ref() {
            Some(state) => state,
            None => {
                let Some((&clue, sub_clues)) = goal.clues.split_first() else {
                    return Ok(match goal.cells.iter().any(|&cell| cell == Cell::Filled) {
                        true => 0,
                        false => 1,
                    });
                };

                let upper_bound = goal
                    .cells
                    .iter()
                    .position(|&cell| cell == Cell::Filled)
                    .map(|n| n + 1)
                    .unwrap_or(goal.cells.len());

                let matching_block_positions = goal
                    .cells
                    .windows(clue + 1)
                    .enumerate()
                    .take(upper_bound)
                    .filter(|(_i, candidate)| block_fits_here(candidate))
                    .map(|(i, _block)| i)
                    .collect();

                state.insert(TaskState {
                    clue,
                    sub_clues,
                    matching_block_positions,
                })
            }
        };

        state.matching_block_positions.iter().try_fold(0, |sum, i| {
            let sub_cells = &goal.cells[i + state.clue + 1..];
            // Trim all empty space off the front of the cells
            let next_possible_spot = sub_cells
                .iter()
                .position(|&cell| matches!(cell, Cell::Unknown | Cell::Filled))
                .unwrap_or(sub_cells.len());

            let sub_cells = &sub_cells[next_possible_spot..];

            let count = subtasker.solve(RowState {
                cells: sub_cells,
                clues: state.sub_clues,
            })?;

            Ok(sum + count)
        })
    }
}

fn parse_row(input: &str) -> ITResult<&str, Row> {
    parser! {
        collect_separated_terminated(parse_cell, success(()), space1) => cells,
        collect_separated_terminated(
            digit1.parse_from_str_cut::<usize>(),
            char(','),
            multispace1.or(multispace0.terminated(eof)),
        ) => clues;
        Row{cells, clues}
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    rows: Vec<Row>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_row, success(()), eof)
        .map(|rows| Input { rows })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> anyhow::Result<usize> {
    Ok(input
        .rows
        .into_par_iter()
        .map(|row| row.normalize())
        .map(|row| {
            execute(
                RowState {
                    cells: &row.cells,
                    clues: &row.clues,
                },
                &CountPossibleArrangements,
                HashMap::new(),
            )
            .expect("shouldn't be possible to have a circular dependency")
        })
        .sum())
}

pub fn part2(input: Input) -> anyhow::Result<usize> {
    Ok(input
        .rows
        .into_par_iter()
        .map(|row| row.unfold())
        .map(|row| row.normalize())
        .map(|row| {
            execute(
                RowState {
                    cells: &row.cells,
                    clues: &row.clues,
                },
                &CountPossibleArrangements,
                HashMap::new(),
            )
            .expect("shouldn't be possible to have a circular dependency")
        })
        .sum())
}
