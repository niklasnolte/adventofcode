function findConns(from, conns)
  return [y for (x,y) in conns if x == from]
end

function extendPath(path, conns, isValid)
  if path[end] == "end"
    return [path]
  end
  newpaths = [vcat(path, [next]) for next in findConns(path[end], conns)
               if isValid(next, path)]
  return vcat([extendPath(np, conns, isValid) for np in newpaths]...)
end


function part1(conns)
  function validMove(move, path)
    return isuppercase(move[1]) || !(move in path)
  end
  paths = ["start"]
  extendPath(paths, conns, validMove) |> length |> println
end

function part2(conns)
  function validMove2(move, path)
    if isuppercase(move[1])
      return true
    end
    if move == "start"
      return false
    end
    if !(move in path)
      return true
    end
    smallCaves = [c for c in path if islowercase(c[1])]    
    if length(smallCaves) == length(unique(smallCaves))
      return true
    end
    return false
  end
  paths = ["start"]
  extendPath(paths, conns, validMove2) |> length |> println
end

function main()
  conns = split.(readlines("input12.txt"), "-")
  conns = vcat(conns, reverse.(conns))
  part1(conns)
  part2(conns)
end

main()
