struct Range {
    min: u32,
    max: u32,
}

fn parse_input(input: &String) -> Vec<(Range, Range)> {
    input.lines().map(parse_line).collect()
}

fn parse_line(line: &str) -> (Range, Range) {
    // split in left and right by ","
    let (left, right) = line.split_once(",").unwrap();
    // split left and right by "-"
    let (lmin, lmax) = left.split_once("-").unwrap();
    let (rmin, rmax) = right.split_once("-").unwrap();
    let [lmin, lmax, rmin, rmax] = [lmin, lmax, rmin, rmax].map(|s| s.parse::<u32>().unwrap());
    (
        (Range {
            min: lmin,
            max: lmax,
        }),
        (Range {
            min: rmin,
            max: rmax,
        }),
    )
}

fn part1(input: &Vec<(Range, Range)>) -> u32 {
    input
        .iter()
        .filter(|(l, r)| (l.min <= r.min && l.max >= r.max) || (l.min >= r.min && l.max <= r.max))
        .count() as u32
}

fn part2(input: &Vec<(Range, Range)>) -> u32 {
    // check how many ranges overlap
    input
        .iter()
        .filter(|(l, r)| l.min <= r.max && r.min <= l.max)
        .count() as u32
}

fn main() {
    let input = std::fs::read_to_string("inputs/04").unwrap();
    let input = &parse_input(&input);
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}
