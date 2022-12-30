type Stack = Vec<char>;
type Movement = (usize, usize, usize);

fn parse_stacks(input: &str) -> Vec<Stack> {
    let mut lines: Vec<&str> = input.lines().collect();
    let nstacks = 0..(lines.pop().unwrap().len() / 4 + 1);
    // parse stacks
    let stacks: Vec<Vec<char>> = nstacks
        .map(|i| {
            lines
                .iter()
                .map(|l| l.chars().nth(4 * i + 1).unwrap_or(' '))
                .filter(|c| *c != ' ')
                .rev()
                .collect()
        })
        .collect();

    return stacks;
}

fn execute_move_seq(stacks: &mut Vec<Stack>, movement: &Movement) {
    let (n, from, to) = movement;
    for _i in 0..*n {
        let c = stacks[*from-1].pop().unwrap(); // existence is guaranteed
        stacks[*to-1].push(c);
    }
}

fn execute_move_multi(stacks: &mut Vec<Stack>, movement: &Movement) {
    let (n, from, to) = movement;
    let from_stack = &mut stacks[*from-1];
    let mut tmp: Vec<char> = from_stack.drain(from_stack.len() - n..).collect();
    let to_stack = &mut stacks[*to-1];
    to_stack.append(&mut tmp);
}

fn parse_movements(input: &str) -> Vec<Movement> {
    let lines = input.lines();

    // parse all numbers in each line
    // example line: "move 3 from 2 to 1" -> (3, 2, 1)
    let movements: Vec<Movement> = lines
        .map(|l| {
            let mut numbers: Vec<usize> = l
                .split_whitespace()
                .filter_map(|s| s.parse::<usize>().ok())
                .collect();
            let to = numbers.pop().unwrap();
            let from = numbers.pop().unwrap();
            let n = numbers.pop().unwrap();
            return (n, from, to);
        })
        .collect();
    return movements;
}

fn parse_input(input: &String) -> (Vec<Stack>, Vec<Movement>) {
    // split by "\n\n" and unpack into stacks and movements
    let (stacks, movements) = input.split_once("\n\n").unwrap();
    let stacks = parse_stacks(stacks);
    let movements = parse_movements(movements);
    return (stacks, movements);
}

fn part1(mut stacks: Vec<Stack>, movements: &Vec<Movement>) -> String {
  for m in movements {
      execute_move_seq(&mut stacks, m);
  }
  // last element of each stack into one string
  return stacks.iter().map(|s| s.last().unwrap_or(&' ')).collect();
}

fn part2(mut stacks: Vec<Stack>, movements: &Vec<Movement>) -> String {
  for m in movements {
      execute_move_multi(&mut stacks, m);
  }
  // last element of each stack into one string
  return stacks.iter().map(|s| s.last().unwrap_or(&' ')).collect();
}

fn main() {
    let input = std::fs::read_to_string("inputs/05").unwrap();

    let (stacks, movements) = parse_input(&input);

    println!("Part 1: {}", part1(stacks.clone(), &movements));
    println!("Part 2: {}", part2(stacks.clone(), &movements));
}
