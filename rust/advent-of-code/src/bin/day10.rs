use std::io;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ParseError {
    Paren,
    Curly,
    Square,
    Angle,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct BracketCount {
    paren: i64,
    square: i64,
    curly: i64,
    angle: i64,
}

const INIT_BRACKET_COUNT: BracketCount = BracketCount {
    paren: 0,
    square: 0,
    curly: 0,
    angle: 0
};

fn read(reader: BracketCount, input: char) -> BracketCount {
    match input {
        '(' => { BracketCount { paren: reader.paren + 1, ..reader } },
        ')' => { BracketCount { paren: reader.paren - 1, ..reader } },
        '{' => { BracketCount { curly: reader.curly + 1, ..reader } },
        '}' => { BracketCount { curly: reader.curly - 1, ..reader } },
        '[' => { BracketCount { square: reader.square + 1, ..reader } },
        ']' => { BracketCount { square: reader.square - 1, ..reader } },
        '<' => { BracketCount { angle: reader.angle + 1, ..reader } },
        '>' => { BracketCount { angle: reader.angle - 1, ..reader } },
        _ => { panic!("failed") }
    }
}

fn check_error(count: BracketCount) -> Option<ParseError> {
    match count {
        BracketCount { paren: 1, .. } | BracketCount { paren: -1, .. } => Some(ParseError::Paren),
        BracketCount { square: 1, .. } | BracketCount { square: -1, .. } => Some(ParseError::Square),
        BracketCount { curly: 1, .. } | BracketCount { curly: -1, .. } => Some(ParseError::Curly),
        BracketCount { angle: 1, ..  } | BracketCount { angle: -1, ..  } => Some(ParseError::Angle),
        INIT_BRACKET_COUNT => None,
        _ => panic!("{:?}", count)
    }
}

fn score(error: ParseError) -> u64 {
    match error {
        ParseError::Paren => 3,
        ParseError::Square => 57,
        ParseError::Curly => 1197,
        ParseError::Angle => 25137,
    }
}

// #[allow(unused)]
// fn problem1(line: &str) -> Option<u64> {
//     let balance = line
//         .chars()
//         .fold(INIT_BRACKET_COUNT, read);

//     let check = check_error(balance);

//     match check {
//         Some(error) => Some(score(error)),
//         None => None
//     }
// }

use std::ops::ControlFlow;

#[allow(unused)]
fn problem1(line: &str) -> ControlFlow<ParseError, BracketCount>{
    let balance = line
        .chars()
        .try_fold(INIT_BRACKET_COUNT, |count, x| {
            if let Some(error) = check_error(count.clone()) {
                println!("{:?}", count);
                ControlFlow::Break(error)
            } else {
                ControlFlow::Continue(read(count, x))
            }
        });

    return balance;
}

fn main() -> io::Result<()> {
    // read stdin
    let mut buf = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buf)?;

    let example = include_str!("example-day10.txt");
    let example_answers = example.lines().map(problem1).collect::<Vec<_>>();
    println!("{:?}", example_answers);

    // let answer1 = buf.lines().map(problem1).collect::<Vec<_>>();
    // let answer1 = buf.lines().map(problem1).collect::<Vec<_>>();
    // solve question 1
    // let question1 = solve(buf.as_str())?;
    // println!("{:?}", answer1);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{read, check_error, INIT_BRACKET_COUNT, BracketCount, ParseError};
    use std::io;

    #[test]
    fn simple_parse() {
        let input = "{([<>])}";
        assert_eq!(
            input.chars().fold(INIT_BRACKET_COUNT, read),
            INIT_BRACKET_COUNT
        );
    }

    #[test]
    fn simple_unbalanced() {
        let input = "{([<])}";
        assert_eq!(
            input.chars().fold(INIT_BRACKET_COUNT, read),
            BracketCount { paren: 0, square: 0, curly: 0, angle: 1 }
        );
    }

    #[test]
    fn check_unbalanced_error() {
        let input = "{([<])}";
        assert_eq!(
            check_error(input.chars().fold(INIT_BRACKET_COUNT, read)),
            Some(ParseError::Angle)
        );
    }
}
