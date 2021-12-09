import Pipe: @pipe

read_input() = @pipe readlines("input9.txt") .|> collect .|> parse.(Int32, _)

function neighbors(x::Integer, y::Integer, grid::Matrix{Int32})::Vector{CartesianIndex{2}}
  #assume at least size (2,2)
  if x == 1
    if y == 1
      idxs = [(2, 1), (1, 2)]
    elseif y == size(grid, 2)
      idxs = [(2, y), (1, y - 1)]
    else
      idxs = [(2, y), (1, y - 1), (1, y + 1)]
    end
  elseif x == size(grid, 1)
    if y == 1
      idxs = [(x - 1, 1), (x, 2)]
    elseif y == size(grid, 2)
      idxs = [(x - 1, y), (x, y - 1)]
    else
      idxs = [(x - 1, y), (x, y - 1), (x, y + 1)]
    end
  elseif y == 1
    idxs = [(x - 1, 1), (x, 2), (x + 1, 1)]
  elseif y == size(grid, 2)
    idxs = [(x - 1, y), (x, y - 1), (x + 1, y)]
  else
    idxs = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]
  end
  return idxs .|> CartesianIndex
end

isPit((x, y), grid) = all(grid[neighbors(x, y, grid)] .> grid[x, y])

function getPits(grid::Matrix{Int32})::Vector{CartesianIndex{2}}
  idxs = @pipe CartesianIndices(size(grid)) .|> Tuple
  return filter(x -> isPit(x, grid), idxs) .|> CartesianIndex
end

function part1(grid::Matrix{Int32})
  pits = getPits(grid)
  grid[pits] .+ 1 |> sum |> println
end

function floodFill(start, grid::Matrix{Int32})::Int32
  filled = Set([start])
  frontier = Set([start])
  while true
    for F in frontier
      ns = neighbors(F..., grid)
      next = filter(i -> grid[i] < 9, ns) .|> Tuple
      frontier = union(frontier, next)
    end
    frontier = setdiff(frontier, filled)
    filled = union(filled, frontier)
    if frontier == Set()
      break
    end
  end
  return length(filled)
end

take3Biggest(x) = sort(x)[end-2:end]

function part2(grid)
  @pipe getPits(grid) .|> Tuple .|> floodFill(_, grid) |> take3Biggest |> prod |> println
end

function main()
  input = read_input()
  grid = reshape(vcat(input...), :, length(input))
  part1(grid)
  part2(grid)
end

main()
