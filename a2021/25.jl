Point = Tuple{Int64, Int64}
@enum CC South East

Grid = Tuple{Dict{Point, CC}, Int64, Int64}

function getInput()
  inp = readlines("input25.txt")
  lx = length(inp)
  ly = length(inp[1])
  grid = Dict{Point, CC}()
  for (i,line) in enumerate(inp)
    for (j,v) in enumerate(line)
      if v == 'v'
        grid[(i-1,j-1)] = South
      elseif v == '>'
        grid[(i-1,j-1)] = East
      end
    end
  end
  return (grid, lx, ly)
end

function iterateGrid(grid::Grid)
  (g,w,h) = grid
  ng = copy(g)
  # move easts
  for ((i,j),cc) in g
    if cc == East
      next = (i,(j+1)%h)
      if !(next in keys(g))
        ng[next] = East
        delete!(ng, (i,j))
      end
    end
  end
  g = copy(ng)
  # move souths
  for ((i,j),cc) in g
    if cc == South
      next = ((i+1)%w,j)
      if !(next in keys(g))
        ng[next] = South
        delete!(ng, (i,j))
      end
    end
  end
  return (ng,w,h)
end

function part1(grid::Grid)
  ccs = copy(grid[1])
  i = 1
  while true
    grid = iterateGrid(grid)  
    if grid[1] == ccs
      break
    else
      ccs = copy(grid[1])
      i += 1
    end
  end
  println(i)
end

function printGrid(grid)
  (g,w,h) = grid
  for i in 0:w-1
    for j in 0:h-1
      item = get(g, (i,j), '.')
      if item == South
        print('v')
      elseif item == East
        print('>')
      else
        print('.')
      end
    end
    println()
  end
  println()
end

function main()
  grid = getInput()
  part1(grid)
end

main()
