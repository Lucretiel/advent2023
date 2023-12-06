use std::{collections::HashMap, convert::Infallible};

use gridly::prelude::*;
use itertools::Itertools as _;

use crate::library::{Definitely, IterExt};

#[derive(Debug, Copy, Clone)]
struct Number {
    value: i32,
    width: Columns,
}

#[derive(Debug, Clone, Copy)]
enum Cell {
    Number(Number),
    Component(u8),
}

#[derive(Debug)]
pub struct Input {
    map: HashMap<Location, Cell>,
}

impl From<&str> for Input {
    fn from(value: &str) -> Self {
        let map = value
            .trim()
            // For each line in the input...
            .lines()
            // Get the row number for this line
            .with_rows(Row(0))
            // Compute all the cells in the line
            .flat_map(|(row, line)| {
                line
                    // For each byte in the line
                    .bytes()
                    // Compute an optional cell, based on the line, where a cell is
                    // either a number of width 1, a component, or None
                    .map(|b| match b {
                        b'.' => None,
                        b if b.is_ascii_digit() => Some(Cell::Number(Number {
                            value: (b - b'0') as i32,
                            width: Columns(1),
                        })),
                        b => Some(Cell::Component(b)),
                    })
                    // Add a column number to each cell
                    .with_columns(Column(0))
                    // Use coalesce to combine adjacent number cells into a single
                    // larger number cell, using the column of the original cell
                    .coalesce(|(col1, cell1), (col2, cell2)| match (cell1, cell2) {
                        (Some(Cell::Number(cell1)), Some(Cell::Number(cell2))) => Ok((
                            col1,
                            Some(Cell::Number(Number {
                                width: cell1.width + cell2.width,
                                value: cell1.value * 10 + cell2.value,
                            })),
                        )),
                        (cell1, cell2) => Err(((col1, cell1), (col2, cell2))),
                    })
                    // Filter out all the empty cells. This must be done *after*
                    // the coalesce, to ensure that two numbers separated by
                    // only empty space aren't joined together.
                    .filter_map(|(column, cell)| cell.map(|cell| (column, cell)))
                    // For each final cell, combine the Row and Column into
                    // Location.
                    .map(move |(col, cell)| (row + col, cell))
            })
            // Collect all the cells into a hash map
            .collect();

        Input { map }
    }
}

/// Create an iterator over all the locations (including diagonals) that are
/// adjacent to the rectangle described by the root and the width:
///
/// ```text
/// aaaaaa
/// ar...a
/// aaaaaa
/// ```
///
/// given that `r` is the `root`, and `.` is the locations to its right in the
/// width, return all of the `a` locations, in an unspecified order.
fn compute_adjacencies(root: Location, width: Columns) -> impl Iterator<Item = Location> {
    let left_spot = root.step(Left);
    let base1 = left_spot.step(Up);
    let base2 = left_spot.step(Down);

    let row1 = LocationRange::new(base1.row, base1.column.span(width + Columns(2)));
    let row2 = LocationRange::new(base2.row, base2.column.span(width + Columns(2)));

    let right_spot = left_spot + width + Right;

    [row1, row2]
        .into_iter()
        .flatten()
        .chain([left_spot, right_spot])
}

pub fn part1(input: Input) -> Definitely<i32> {
    Ok(input
        .map
        .iter()
        .filter_map(|(&location, &cell)| match cell {
            Cell::Number(number) => Some((location, number)),
            Cell::Component(_) => None,
        })
        .filter(|&(location, number)| {
            compute_adjacencies(location, number.width)
                .any(|adj| matches!(input.map.get(&adj), Some(&Cell::Component(_))))
        })
        .map(|(_, number)| number.value)
        .sum())
}

pub fn part2(input: Input) -> Definitely<i32> {
    Ok(input
        .map
        .iter()
        // Find the locations of all the gears
        .filter_map(|(&location, &cell)| match cell {
            Cell::Component(b'*') => Some(location),
            _ => None,
        })
        // For each gear, find its gear ratio
        .filter_map(|gear_location| {
            // We need to find part adjacent part numbers. We search the 8
            // adjacent locations, and (based on the assumption that all part
            // numbrs are no more than 5 digits wide) search backwards a few
            // spaces those 5.
            //
            // We'll confirm that every candidate part number is actually
            // adjacent to the gear, so it's okay if the candidate set is a bit
            // too large, so long as it never has false negatives.
            let mut nearby_parts = [
                gear_location.left(5).above(1).span_over(Columns(7)),
                gear_location.left(5).span_over(Columns(7)),
                gear_location.left(5).below(1).span_over(Columns(7)),
            ]
            .into_iter()
            .flatten()
            .filter_map(|candidate| {
                let Some(&Cell::Number(number)) = input.map.get(&candidate) else {
                    return None;
                };

                // We found a part. Check that it's actuallly adjacent to the gear.
                compute_adjacencies(candidate, number.width)
                    .any(|adj| adj == gear_location)
                    .then_some(number)
            });

            // Check that there are precisely two parts in the nearby parts
            let part1 = nearby_parts.next()?;
            let part2 = nearby_parts.exactly_one().ok()?;

            // If there are, find the gear ratio
            Some(part1.value * part2.value)
        })
        .sum())
}
