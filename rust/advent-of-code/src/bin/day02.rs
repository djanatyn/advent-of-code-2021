use std::io;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum Direction {
    Forward(u64),
    Down(u64),
    Up(u64),
}

#[derive(Debug)]
struct Position {
    depth: u64,
    horizontal: u64,
}

fn parse(line: &str) -> io::Result<Direction> {
    // split direction + magnitude
    let split: Vec<&str> = line.split(' ').collect();

    // there should be two elements
    assert_eq!(split.len(), 2);

    // first element is direction
    match split[0] {
        "forward" => Ok(Direction::Forward(split[1].parse::<u64>().unwrap())),
        "down" => Ok(Direction::Down(split[1].parse::<u64>().unwrap())),
        "up" => Ok(Direction::Up(split[1].parse::<u64>().unwrap())),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "failed to parse",
        )),
    }
}

fn navigate(pos: Position, dir: Direction) -> Position {
    match dir {
        Direction::Forward(n) => Position {
            depth: pos.depth,
            horizontal: pos.horizontal + n,
        },
        Direction::Down(n) => Position {
            depth: pos.depth + n,
            horizontal: pos.horizontal,
        },
        Direction::Up(n) => Position {
            depth: pos.depth - n,
            horizontal: pos.horizontal,
        },
    }
}

#[allow(unused)]
fn solve(puzzle: &str) -> io::Result<u64> {
    let start = Position {
        depth: 0,
        horizontal: 0,
    };

    let answer = puzzle
        .lines()
        .filter_map(|dir| parse(dir).ok())
        .fold(start, navigate);

    Ok(answer.horizontal * answer.depth)
}

fn main() -> io::Result<()> {
    // read stdin
    let mut buf = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buf)?;

    // solve question 1
    let question1 = solve(buf.as_str())?;
    println!("{}", question1);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{parse, solve, Direction};
    use std::io;

    #[test]
    fn example() -> io::Result<()> {
        let result = solve(include_str!("example.txt"))?;
        assert_eq!(result, 150);

        Ok(())
    }

    #[test]
    fn parse_forward() {
        assert_eq!(parse("forward 5").unwrap(), Direction::Forward(5))
    }

    #[test]
    fn parse_down() {
        assert_eq!(parse("down 3").unwrap(), Direction::Down(3))
    }

    #[test]
    fn parse_up() {
        assert_eq!(parse("up 7").unwrap(), Direction::Up(7))
    }
}
