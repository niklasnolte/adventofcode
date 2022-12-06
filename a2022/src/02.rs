fn read_input() -> Vec<(char, char)> {
    return std::fs::read_to_string("inputs/02")
        .unwrap()
        .lines()
        .map(|x| x.chars())
        // take element 0 and 2
        .map(|mut x| (x.nth(0).unwrap(), x.nth(1).unwrap()))
        .collect();
}

fn score_p1(input: (char, char)) -> u32 {
    // A = rock, B = paper, C = scissors
    // X = rock, Y = paper, Z = scissors
    // 6 for win, 3 for draw, 0 for loss
    return match input {
        ('A', 'X') => 4,
        ('A', 'Y') => 8,
        ('A', 'Z') => 3,
        ('B', 'X') => 1,
        ('B', 'Y') => 5,
        ('B', 'Z') => 9,
        ('C', 'X') => 7,
        ('C', 'Y') => 2,
        ('C', 'Z') => 6,
        _ => panic!("Invalid input"),
    };
}

fn score_p2(input: (char, char)) -> u32 {
    // A = rock, B = paper, C = scissors
    // X = loose, Y = draw, Z = win
    // 6 for win, 3 for draw, 0 for loss
    return match input {
        ('B', 'X') => 1,
        ('C', 'X') => 2,
        ('A', 'X') => 3,
        ('A', 'Y') => 4,
        ('B', 'Y') => 5,
        ('C', 'Y') => 6,
        ('C', 'Z') => 7,
        ('A', 'Z') => 8,
        ('B', 'Z') => 9,
        _ => panic!("Invalid input"),
    };
}

fn part1(input: &Vec<(char, char)>) -> u32 {
    return input.iter().map(|x| score_p1(*x)).sum();
}

fn part2(input: &Vec<(char, char)>) -> u32 {
    return input.iter().map(|x| score_p2(*x)).sum();
}

fn main() {
    let input = read_input();
    print!("Part 1: {}\n", part1(&input));
    print!("Part 2: {}\n", part2(&input));
}
