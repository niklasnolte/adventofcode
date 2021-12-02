@enum Dir FW UP DN

function parse_dir(dir)::Dir
  if dir == "forward"
    return FW
  elseif dir == "up"
    return UP
  elseif dir == "down"
    return DN
  end
end

function parse_instr(x::String)::Tuple{Dir, Int32}
  dirstr, magstr = split(x, " ")
  return parse_dir(dirstr), parse(Int32, magstr)
end

function get_input()::Vector{Tuple{Dir, Int32}}
  input = readlines("input2.txt")
  return parse_instr.(input)
end

function part1(input::Vector{Tuple{Dir, Int32}})::Int32
  x = 0
  y = 0
  for (dir,mag) in input
    if dir == FW
      x += mag
    elseif dir == UP
      y -= mag
    elseif dir == DN
      y += mag
    end
  end
  return x*y
end

function part2(input::Vector{Tuple{Dir, Int32}})::Int32
  x = 0
  y = 0
  aim = 0
  for (dir,mag) in input
    if dir == FW
      x += mag
      y += aim * mag
    elseif dir == UP
      aim -= mag
    elseif dir == DN
      aim += mag
    end
  end
  return x*y
end

function main()
  inp = get_input()
  println(part1(inp))
  println(part2(inp))
end

main()
