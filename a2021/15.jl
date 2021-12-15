import Pipe:@pipe
import DataStructures:PriorityQueue, dequeue!, enqueue!, peek


function neighbors((x,y), d)
  ns = [[x-1, y], [x+1, y], [x, y-1], [x, y+1]]
  return [(x,y) for (x,y) in ns if (x,y) in keys(d)]
end

function shortestPath(grid, from, final)
  dists = PriorityQueue{Tuple{Int64, Int64}, Int64}(
            [(k, Integer(10e10)) for k in keys(grid)]
          )
  dists[from] = 0
  while length(dists) != 0
    (current, cdist) = peek(dists)
    dequeue!(dists)
    ns = neighbors(current, dists)
    if final in ns
      return cdist + grid[final]
    end
    for n in ns
        dists[n] = min(dists[n], cdist + grid[n])
    end
  end
  throw("no path found")
end

function part1(input)
  dims = (length(input), length(input[1]))
  grid = Dict([((x,y), input[x][y]) for x in 1:dims[1]
            for y in 1:dims[2]])
  shortestPath(grid, (1,1), dims) |> println
end

function part2(input)
  dims = (length(input), length(input[1]))
  grid = Dict([((x,y), input[x][y] - 1) for x in 1:dims[1]
            for y in 1:dims[2]])
  ks = [k for k in keys(grid)]
  for X in 0:4
    for Y in 0:4
      for (x,y) in ks
        grid[(x+X*dims[1], y+Y*dims[2])] = grid[(x,y)] + X + Y
      end
    end
  end
  for (x,y) in keys(grid)
    grid[(x,y)] = grid[(x,y)] % 9 + 1
  end
  shortestPath(grid, (1,1), dims.*5) |> println
end

function main()
  lines = readlines("input15.txt")
  lines = @pipe collect.(lines) .|> parse.(Int32, _)
  part1(lines)
  part2(lines)
end

main()
