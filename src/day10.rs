use anyhow::Context;
use gridly::prelude::*;
use gridly_grids::VecGrid;

#[derive(Debug, Clone, Copy)]
enum Pipe {
    Vertical,
    Horizontal,
    UpLeft,
    UpRight,
    DownLeft,
    DownRight,
}

impl Pipe {
    pub fn from_byte(b: u8) -> Option<Self> {
        use Pipe::*;

        match b {
            b'|' => Some(Vertical),
            b'-' => Some(Horizontal),
            b'J' => Some(UpLeft),
            b'L' => Some(UpRight),
            b'7' => Some(DownLeft),
            b'F' => Some(DownRight),
            _ => None,
        }
    }

    pub fn directions(self) -> [Direction; 2] {
        match self {
            Pipe::Vertical => [Up, Down],
            Pipe::Horizontal => [Left, Right],
            Pipe::UpLeft => [Up, Left],
            Pipe::UpRight => [Up, Right],
            Pipe::DownLeft => [Down, Left],
            Pipe::DownRight => [Down, Right],
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Cell {
    Start,
    Pipe(Pipe),
}

impl Cell {
    pub fn from_byte(b: u8) -> Option<Self> {
        match b {
            b'S' => Some(Cell::Start),
            b => Pipe::from_byte(b).map(Cell::Pipe),
        }
    }

    pub fn pipe(self) -> Option<Pipe> {
        match self {
            Cell::Start => None,
            Cell::Pipe(pipe) => Some(pipe),
        }
    }
}

#[derive(Debug)]
pub struct Input {
    map: VecGrid<Option<Cell>>,
}

impl Input {
    fn start_location(&self) -> Option<Location> {
        self.map.rows().iter().find_map(|row| {
            row.iter_with_locations()
                .find(|&(_loc, &cell)| matches!(cell, Some(Cell::Start)))
                .map(|(loc, _cell)| loc)
        })
    }
}

impl TryFrom<&str> for Input {
    type Error = anyhow::Error;

    fn try_from(input: &str) -> Result<Self, Self::Error> {
        VecGrid::new_from_rows(
            input
                .lines()
                .map(|line| line.as_bytes().iter().map(|&b| Cell::from_byte(b))),
        )
        .context("failed to create grid")
        .map(|map| Input { map })
    }
}

fn trace_map<T>(
    input: &Input,
    mut state: T,
    step_func: impl Fn(T, Direction, Location, Pipe) -> T,
) -> anyhow::Result<T> {
    let start_location = input
        .start_location()
        .context("No start location in input")?;

    let (mut step_direction, mut current_location, mut current_pipe) = EACH_DIRECTION
        .iter()
        .find_map(|&direction| {
            let target = start_location + direction;
            let &cell = input.map.get(start_location + direction).ok()?;
            let pipe = cell?.pipe()?;

            // If this pipe points back at the start location
            if pipe
                .directions()
                .iter()
                .any(|&pipe_dir| pipe_dir == direction.reverse())
            {
                Some((direction, target, pipe))
            } else {
                None
            }
        })
        .context("no pipes connected to the start location")?;

    loop {
        state = step_func(state, step_direction, current_location, current_pipe);

        step_direction = current_pipe
            .directions()
            .iter()
            .copied()
            .find(|&dir| dir != step_direction.reverse())
            .expect("pipes have 2 different directions");

        current_location += step_direction;

        let current_cell = input
            .map
            .get(current_location)
            .ok()
            .context("pipe led off the edge of the map")?
            .context("pipe led to an empty cell")?;

        current_pipe = match current_cell {
            Cell::Start => break Ok(state),
            Cell::Pipe(pipe) => pipe,
        };
    }
}

pub fn part1(input: Input) -> anyhow::Result<i64> {
    trace_map(&input, 1, |distance, _, _, _| distance + 1).map(|distance| distance / 2)
}

pub fn part2(input: Input) -> anyhow::Result<i64> {
    trace_map(
        &input,
        (1i64, 0i64),
        |(area, height), direction, _, _| match direction {
            Up => (area, height + 1),
            Down => (area - 1, height - 1),
            Right => (area + height, height),
            Left => (area - (height + 1), height),
        },
    )
    .map(|(area, _height)| area.abs())
}
