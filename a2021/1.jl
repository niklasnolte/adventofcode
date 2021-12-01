function part1(input::Vector{Int32})
  println(sum(diff(input) .> 0))
end

function part2(input::Vector{Int32})
  n = lastindex(input)
  threesum = input[1:n-2] .+ input[2:n-1] .+ input[3:n]
  part1(threesum)
end

function get_input()::Vector{Int32}
  input = readlines("input1.txt")
  input = parse.(Int32, input)
  return input
end

function main()
  input = get_input()
  part1(input)
  part2(input)
end

main()
