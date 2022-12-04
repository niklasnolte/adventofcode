fn get_calories<NUMBER>(elf: &str) -> NUMBER
where
    NUMBER: std::str::FromStr + std::iter::Sum,
    <NUMBER as std::str::FromStr>::Err: std::fmt::Debug,
{
    return elf.split("\n").map(|x| x.parse::<NUMBER>().unwrap()).sum();
}

fn part2(input: &String) -> i32 {
    let elves = input.split("\n\n").collect::<Vec<&str>>();
    // maintain three biggest calories with a min-heap
    let mut three_biggest = std::collections::BinaryHeap::<i32>::new();

    for elf in elves[..3].iter() {
        let calories: i32 = get_calories::<i32>(elf);
        three_biggest.push(-calories); // max heap
    }

    for elf in elves[3..elves.len() - 1].iter() {
        let calories: i32 = get_calories::<i32>(elf);
        three_biggest.push(-calories);
        three_biggest.pop();
    }

    return -three_biggest.iter().sum::<i32>();
}

fn part1(input: &String) -> u32 {
    let elves = input.split("\n\n").collect::<Vec<&str>>();
    // remove last element from elves
    let mut max_calories = 0;

    for elf in elves[..elves.len() - 1].iter() {
        let calories = get_calories::<u32>(elf);
        max_calories = std::cmp::max(max_calories, calories);
    }
    return max_calories;
}

fn main() {
    let input = std::fs::read_to_string("inputs/01").unwrap();
    print!("Part 1: {}\n", part1(&input));
    print!("Part 1: {}\n", part2(&input));
}
