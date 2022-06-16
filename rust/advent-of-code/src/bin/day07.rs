use std::collections::HashMap;

fn main() {
    let input = include_str!("day07.txt");
    let crabs = parse(input);

    let solution = find_alignment(input);

    println!("{solution}");
}

/// Parse input into crab positions.
fn parse(input: &str) -> Vec<i32> {
    input
        .trim()
        .split(',')
        .map(|num| dbg!(num).parse::<i32>().expect("failed to parse"))
        .collect::<Vec<_>>()
}

/// See how much fuel is needed to align all crabs to a given position.
fn check_fuel_needed(position_to_align: i32, crabs: &Vec<i32>) -> i32 {
    let mut fuel_used = 0;

    for crab in crabs {
        let difference = (crab - position_to_align).abs();

        fuel_used += difference;
    }

    fuel_used
}

/// solve the problem, taking the input string, returning the solution
/// TODO: we've conflated parsing and finding alignment
/// run parsing elsewhere
fn find_alignment(input: &str) -> i32 {
    let crabs = parse(input);

    // we are going to check every alignment value between min and max
    let min_crab_pos = *(crabs.iter().min().expect("could not find min"));
    let max_crab_pos = *(crabs.iter().max().expect("could not find max"));

    // keys are crab positions to align on
    // values are fuel required
    let mut fuel_map: HashMap<i32, i32> = HashMap::new();

    // check every possible crab position and record fuel required in fuel_map
    for pos_to_check in min_crab_pos..max_crab_pos {
        let fuel_needed = check_fuel_needed(pos_to_check, &crabs);

        fuel_map.insert(pos_to_check, fuel_needed);
    }

    dbg!(&fuel_map);

    // we need to find the key with the lowest value
    *fuel_map
        .values()
        .min()
        .expect("could not find lowest value")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_parse_example() {
        let input = "16,1,2,0,4,2,7,1,2,14";
        assert_eq!(vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14], parse(input));
    }

    #[test]
    /// Make sure that the example takes 37 fuel.
    fn check_fuel_example() {
        let input = "16,1,2,0,4,2,7,1,2,14";
        let crabs: Vec<i32> = parse(input);

        assert_eq!(37, check_fuel_needed(2, &crabs))
    }

    #[test]
    fn example() {
        let input = "16,1,2,0,4,2,7,1,2,14";

        // test that they are aligning on the right position
        assert_eq!(37, find_alignment(input));
    }
}
