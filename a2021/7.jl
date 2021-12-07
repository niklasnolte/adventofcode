import Statistics:mean,median

function part1(positions::Vector{Int32})
  positions .- median(positions) .|> abs |> sum |> Int32 |> println
end

function weightedSum(p::Vector{Int32})::Int32
  return (p .* (p .+ 1)) ./ 2 |> sum
end


function part2(positions::Vector{Int32})
  x = positions .- ceil(mean(positions)) .|> Int32 .|> abs|> weightedSum
  y = positions .- floor(mean(positions)) .|> Int32 .|> abs|> weightedSum
  println(min(x,y))
end

function main()
  positions = parse.(Int32, split(readlines("input7.txt")[1], ","))
  part1(positions)
  part2(positions)
end

main()
