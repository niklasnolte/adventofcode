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

function popfirst!(f, x)
  idx = findfirst(f, x)
  out = x[idx]
  deleteat!(x, idx)
  return out
end

function orderExamples(examples::Vector{String})::Vector{String}
  findlenN(n) = findfirst(x -> length(x) == n, examples)
  reprs = repeat([""], 10)
  # careful! reprs[n+1] refers to digit n
  reprs[[2,8,5,9]] = examples[[2, 3, 4, 7].|>findlenN]
  len5s, len6s = @pipe [5, 6] .|> examples[findall(x -> length(x) == _, examples)]

  unique5s = getUniqueEntries(len5s)
  B = popfirst!(x -> x in reprs[5], unique5s)
  E = unique5s[1]

  reprs[6] = popfirst!(x -> B in x, len5s)
  reprs[3] = popfirst!(x -> E in x, len5s)
  reprs[4] = len5s[1]

  reprs[10] = popfirst!(x -> ~(E in x), len6s)
  reprs[1] = popfirst!(x -> all(n in x for n in reprs[8]), len6s)
  reprs[7] = len6s[1]

  return reprs .|> collect .|> sort .|> join
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
