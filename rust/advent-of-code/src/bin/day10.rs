/// TODO: All you need is a stack, BracketCount can be simplified. All fields besides "scope" aren't required.

use std::io;
use std::io::Read;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Bracket {
    Paren,
    Curly,
    Square,
    Angle,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct BracketCount {
    scope: Vec<Bracket>,
    paren: i64,
    square: i64,
    curly: i64,
    angle: i64,
}

const INIT_BRACKET_COUNT: BracketCount = BracketCount {
    scope: vec![],
    paren: 0,
    square: 0,
    curly: 0,
    angle: 0,
};

fn read(mut reader: BracketCount, input: char) -> Result<BracketCount, Bracket> {
    match input {
        '(' => {
            reader.scope.push(Bracket::Paren);
            Ok(BracketCount {
                paren: reader.paren + 1,
                ..reader
            })
        }
        ')' => match reader.scope.pop() {
            None | Some(Bracket::Paren) => Ok(BracketCount {
                paren: reader.paren - 1,
                ..reader
            }),
            _ => Err(Bracket::Paren),
        },
        '{' => {
            reader.scope.push(Bracket::Curly);
            Ok(BracketCount {
                curly: reader.curly + 1,
                ..reader
            })
        }
        '}' => match reader.scope.pop() {
            None | Some(Bracket::Curly) => Ok(BracketCount {
                curly: reader.curly - 1,
                ..reader
            }),
            _ => Err(Bracket::Curly),
        },
        '[' => {
            reader.scope.push(Bracket::Square);
            Ok(BracketCount {
                square: reader.square + 1,
                ..reader
            })
        }
        ']' => match reader.scope.pop() {
            None | Some(Bracket::Square) => Ok(BracketCount {
                square: reader.square - 1,
                ..reader
            }),
            _ => Err(Bracket::Square),
        },
        '<' => {
            reader.scope.push(Bracket::Angle);
            Ok(BracketCount {
                angle: reader.angle + 1,
                ..reader
            })
        }
        '>' => match reader.scope.pop() {
            None | Some(Bracket::Angle) => Ok(BracketCount {
                angle: reader.angle - 1,
                ..reader
            }),
            _ => Err(Bracket::Angle),
        },
        _ => {
            panic!("failed")
        }
    }
}

fn score(error: &Bracket) -> u64 {
    match error {
        Bracket::Paren => 3,
        Bracket::Square => 57,
        Bracket::Curly => 1197,
        Bracket::Angle => 25137,
    }
}

fn score_part2(leftovers: Vec<Bracket>) -> u64 {
    leftovers.iter().fold(0, |acc, bracket| {
        let addition = match bracket {
            Bracket::Paren => 1,
            Bracket::Square => 2,
            Bracket::Curly => 3,
            Bracket::Angle => 4,
        };
        // println!("({} * 5) + {} ({:?})", acc, addition, bracket);
        (acc * 5) + addition
    })
}

#[allow(unused)]
fn problem1(line: &str) -> Option<u64> {
    let balance = line
        .chars()
        .try_fold(INIT_BRACKET_COUNT, |count, c| read(count, c));

    match balance {
        Ok(_) => None,
        Err(bracket) => Some(score(&bracket)),
    }
}

#[allow(unused)]
fn problem2(line: &str) -> Option<u64> {
    let balance = line
        .chars()
        .try_fold(INIT_BRACKET_COUNT, |count, c| read(count, c));

    match balance {
        Err(bracket) => None,
        Ok(BracketCount { mut scope, .. }) => {
            scope.reverse();
            Some(score_part2(scope))
        }
    }
}

fn main() -> io::Result<()> {
    // read stdin
    let mut buf = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buf)?;

    let example = include_str!("example-day10.txt");
    let example_answers: u64 = example
        .lines()
        .map(problem1)
        .filter_map(|score| score)
        .sum();

    println!("example part 1: {:?}", example_answers);

    let problem1_answer: u64 = buf.lines().map(problem1).filter_map(|score| score).sum();

    println!("part 1: {:?}", problem1_answer);

    let mut example_part2_answers = example
        .lines()
        .map(problem2)
        .filter_map(|leftovers| leftovers)
        .collect::<Vec<_>>();

    example_part2_answers.sort();

    println!(
        "example part 2: {}",
        example_part2_answers[(example_part2_answers.len() - 1) / 2]
    );

    let mut problem2_answers = buf
        .lines()
        .map(problem2)
        .filter_map(|leftovers| leftovers)
        .collect::<Vec<_>>();

    problem2_answers.sort();

    println!(
        "part 2: {}",
        problem2_answers[(problem2_answers.len() - 1) / 2]
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::problem1;

    #[test]
    fn simple_unbalanced() {
        assert_eq!(problem1("{([<])}"), Some(57));
    }
}
