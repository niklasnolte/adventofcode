import Pipe: @pipe
import DataStructures: PriorityQueue, dequeue!, peek

function neighbors((x, y))
  return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
end

function shortestPath(grid, from, final)
  MAX = Integer(10e10)
  dists = Dict(from => 0)
  Q = PriorityQueue(from => 0)

  while length(Q) != 0
    current = dequeue!(Q)
    cdist = dists[current]
    for n in neighbors(current)
      if final == n
        return cdist + grid[final]
      end
      new = cdist + get(grid, n, MAX)
      if new < get(dists, n, MAX)
        dists[n] = new
        Q[n] = new + sum(abs.(final .- n))
      end
    end
  end
  throw("no path found")
end

function part1(input)
  dims = (length(input), length(input[1]))
  grid = Dict([((x, y), input[x][y]) for x = 1:dims[1]
               for y = 1:dims[2]])
  shortestPath(grid, (1, 1), dims) |> println
end

function part2(input)
  dims = (length(input), length(input[1]))
  grid = Dict([((x, y), input[x][y] - 1) for x = 1:dims[1]
               for y = 1:dims[2]])
  ks = [k for k in keys(grid)]
  for X = 0:4
    for Y = 0:4
      for (x, y) in ks
        grid[(x + X * dims[1], y + Y * dims[2])] = grid[(x, y)] + X + Y
      end
    end
  end
  for (x, y) in keys(grid)
    grid[(x, y)] = grid[(x, y)] % 9 + 1
  end
  shortestPath(grid, (1, 1), dims .* 5) |> println
end

function main()
  lines = readlines("input15.txt")
  lines = @pipe collect.(lines) .|> parse.(Int32, _)
  part1(lines)
  part2(lines)
end

main()
