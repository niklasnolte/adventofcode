import StatsBase: countmap
import Pipe: @pipe
const Display = Tuple{Vector{String},Vector{String}}

function parseWireInput(line::String)::Display
  a, b = split(line, " | ")
  return (a, b) .|> split
end

function part1(input::Vector{Display})
  outputs = vcat(getfield.(input, 2)...)
  count((x -> x in [2, 3, 4, 7]), length.(outputs)) |> println
end

getUniqueEntries(x) = [c for c in join(x) if count(x -> x == c, join(x)) == 1]

function orderExamples(examples::Vector{String})::Vector{String}
  findlenN(n) = findfirst(x -> length(x) == n, examples)
  repr1, repr7, repr4, repr8 = examples[[2, 3, 4, 7].|>findlenN]
  len5s = examples[findall(x -> length(x) == 5, examples)]
  unique5s = getUniqueEntries(len5s)
  in4 = unique5s .|> x -> x in repr4
  B = unique5s[in4][1]
  E = unique5s[.~in4][1]
  repr5 = [x for x in len5s if B in x][1]
  repr2 = [x for x in len5s if E in x][1]
  repr3 = [x for x in len5s if ~(x in [repr5, repr2])][1]

  len6s = examples[findall(x -> length(x) == 6, examples)]
  repr9 = [x for x in len6s if ~(E in x)][1]
  repr0 = [x for x in len6s if all(n in x for n in repr7) && ~(x == repr9)][1]
  repr6 = [x for x in len6s if ~(x in [repr0, repr9])][1]

  return [
           repr0,
           repr1,
           repr2,
           repr3,
           repr4,
           repr5,
           repr6,
           repr7,
           repr8,
           repr9
         ] .|> collect .|> sort .|> join
end

function findIndices(map::Vector{String}, digits::Vector{String})#::Int32
  sorted = digits .|> collect .|> sort .|> join
  return @pipe sorted .|> findfirst(x -> x == _, map) .|> (x -> x - 1) .|> string |> join |> parse(Int32, _)
end

function part2(input::Vector{Display})
  examples = getfield.(input, 1)
  outputs = getfield.(input, 2)
  maps = orderExamples.(examples)
  findIndices.(maps, outputs) |> sum |> println
end

function main()
  input = readlines("input8.txt") .|> parseWireInput
  part1(input)
  part2(input)
end

main()
