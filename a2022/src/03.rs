use std::collections::{HashMap, HashSet};

fn parse_input_p1(input: &String) -> Vec<(&str, &str)> {
    return input.lines().map(|x| x.split_at(x.len() / 2)).collect();
}
fn parse_input_p2(input: &String) -> Vec<(&str, &str, &str)> {
    return input
        .lines()
        .collect::<Vec<&str>>()
        .chunks(3)
        .map(|x| (x[0], x[1], x[2]))
        .collect();
}

fn find_common_char(input: Vec<&str>) -> Option<char> {
    let first: HashSet<char> = input[0].chars().collect();
    return input
        .iter()
        .skip(1)
        .map(|x| x.chars().collect::<HashSet<char>>())
        .fold(first, |acc, x| acc.intersection(&x).copied().collect())
        .iter()
        .copied()
        .next();
}

fn letter_to_prio(letter: char) -> i32 {
    let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars();
    let prios = 1..(26 * 2 + 1);
    let letter_to_prio: HashMap<char, i32> = letters.zip(prios).collect();
    return *letter_to_prio.get(&letter).unwrap();
}

fn part1(input: &String) -> i32 {
    let input = parse_input_p1(input);

    return input
        .iter()
        .map(|x| find_common_char(vec![x.0, x.1]).unwrap())
        .map(|x| letter_to_prio(x))
        .sum();
}

fn part2(input: &String) -> i32 {
    let input = parse_input_p2(input);
    return input
        .iter()
        .map(|(a, b, c)| find_common_char(vec![a, b, c]).unwrap())
        .map(|x| letter_to_prio(x))
        .sum();
}

fn main() {
    let input = std::fs::read_to_string("inputs/03").unwrap();
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
