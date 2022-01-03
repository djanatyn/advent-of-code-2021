use std::io;
use std::io::Read;
use std::ops::Add;

/// A coordinate within a grid.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Coord {
    x: i8,
    y: i8,
}

impl Add for Coord {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

/// A two-dimensional grid of octopi, each represented by energy level.
#[derive(Debug, Clone)]
struct Grid {
    height: i8,
    width: i8,
    octopi: Vec<Vec<i8>>,
}

/// When evaluating each step, we keep track of what we've already flashed.
struct Step {
    grid: Grid,
    flashed: Vec<Coord>,
}

/// Parse input into a two-dimensional grid.
fn parse(input: &str) -> Grid {
    let octopi: Vec<Vec<i8>> = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_string().parse::<i8>().unwrap())
                .collect()
        })
        .collect();

    let height = octopi.len().try_into().unwrap();
    let width = height; // assume square

    Grid {
        octopi,
        height,
        width,
    }
}

/// Determine whether a coordinate is valid (within the grid).
fn valid_coord(coord: &Coord, grid: &Grid) -> bool {
    (coord.y >= 0) & (coord.x >= 0) & (coord.y <= grid.height) & (coord.x <= grid.width)
}

/// Return the Moore neighborhood of a coordinate.
fn neighbors(coord: &Coord, grid: &Grid) -> Vec<Coord> {
    let directions = vec![
        Coord { x: 0, y: 1 },   // north
        Coord { x: 0, y: -1 },  // south
        Coord { x: 1, y: 0 },   // east
        Coord { x: -1, y: 0 },  // west
        Coord { x: 1, y: 1 },   // north-east
        Coord { x: -1, y: 1 },  // north-west
        Coord { x: 1, y: -1 },  // south-east
        Coord { x: -1, y: -1 }, // south-west
    ];

    directions
        .iter()
        .map(|offset| *coord + *offset)
        .filter(|potential| valid_coord(potential, grid))
        .collect()
}

/// Determine which octopi should be flashed next (>= 9 energy).
fn next_flash(step: &Step) -> Vec<Coord> {
    step.grid
        .octopi
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, octopus)| {
                    if *octopus >= 9 {
                        Some(Coord {
                            x: x.try_into().unwrap(),
                            y: y.try_into().unwrap(),
                        })
                    } else {
                        None
                    }
                })
                .filter_map(|coord| coord)
        })
        .flatten()
        .filter(|coord| !step.flashed.contains(coord))
        .collect()
}

/// Flash octopi, updating `flashed` field, returning a new Step.
fn flash(before: &Step, mut to_flash: Vec<Coord>) -> Step {
    let mut flashed = before.flashed.clone();
    flashed.append(&mut to_flash);

    let mut grid = before.grid.clone();
    for coord @ Coord { x, y } in to_flash {
        let octopi_x = usize::try_from(x).unwrap();
        let octopi_y = usize::try_from(y).unwrap();

        grid.octopi[octopi_y][octopi_x] -= 9;

        for neighbor in neighbors(&coord, &grid) {
            let neighbor_x = usize::try_from(neighbor.x).unwrap();
            let neighbor_y = usize::try_from(neighbor.y).unwrap();

            grid.octopi[neighbor_y][neighbor_x] += 1;
        }
    }

    Step { flashed, grid }
}

/// Increment every octopus energy level.
fn increment(before: &Step) -> &Step {
    todo!()
}

// given a Step, (containing already-flashed coords, and a grid),
// first increment every octopi. next,
// check for next flashes (not doing already-flashed coords),
// if there are no next flashes, the step is complete
// if there are flashes, flash them, returning a new Step, updating already-flashed,
// repeat until step complete

fn main() -> io::Result<()> {
    // read stdin
    let mut buf = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buf)?;

    todo!();
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    /// Check the first and last row of parsed example.
    fn parse_check_ends() {
        let input = include_str!("example-day11.txt");
        let parsed = parse(input);

        assert_eq!(
            parsed.octopi.first(),
            Some(&vec![5, 4, 8, 3, 1, 4, 3, 2, 2, 3])
        );
        assert_eq!(
            parsed.octopi.last(),
            Some(&vec![5, 2, 8, 3, 7, 5, 1, 5, 2, 6])
        );
    }
}