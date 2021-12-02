use std::io;

fn main() -> io::Result<()> {
    println!("Hello, world!");

    Ok(())
}

#[allow(unused)]
fn solve(path: &str) -> io::Result<u64> {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::solve;
    use std::io;

    #[test]
    fn example() -> io::Result<()> {
        let result = solve(include_str!("example.txt"))?;
        assert_eq!(result, 150);

        Ok(())
    }
}
