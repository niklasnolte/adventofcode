import Pipe:@pipe

Coord = Vector{Int32}
Orientation = Vector{Int32}
Rotation = Int8

function getInput()::Vector{Set{Coord}}
  input = read("input19.txt", String)
  input = @pipe split(input, "\n\n") .|> split(_, "\n")
  input = map(x -> x[2:end], input)
  input[end] = input[end][1:end-1]
  input = map.(x -> [parse(Int32, xi) for xi in split(x, ",")], input)
  [Set(x) for x in input]
end

function rotate(p::Coord, r)::Coord
  x,y,z = p
  if r == 0
    return [x,z,y]
  elseif r == 1
    return [y,x,z]
  elseif r == 2
    return [z,y,x]
  elseif r == 3
    return [z,x,y]
  elseif r == 4
    return [y,z,x]
  elseif r == 5
    return [x,y,z]
    throw("Invalid rotation")
  end
end

function changeRefFrame(point, rot, or, origin)
  point = rotate(point, rot)
  point = point.*or
  return point + origin
end

function findOverlap(points::Set{Coord}, scanner::Set{Coord})::Tuple{Bool, Rotation, Orientation, Coord}
  ROTATIONS = [0,1,2,3,4,5]
  ORIENTATIONS = [[1,1,1], [-1,1,1], [1,-1,1], [1,1,-1], [-1,-1,1],  [-1,1,-1], [1,-1,-1], [-1,-1,-1]]
  for rot in ROTATIONS
    for or in ORIENTATIONS
      oriented = Set([rotate(x, rot).*or for x in scanner])
      for s in oriented
        for p in points
          shifted = Set([o .+ p .- s for o in oriented])
          intersect!(shifted, points)
          if length(shifted) >= 12
            return (true, rot, or, p.-s)
          end
        end
      end
    end
  end
  return (false, 0, [0,0,0], [0,0,0])
end

function part12(input::Vector{Set{Coord}})
  allPoints = copy(input[1])
  # mapped is a tuple of scanners, where
  # each scanner is defined by position, rotation, orientation
  mapped = Dict(0=>([0,0,0], 0, [1,1,1]))
  while length(mapped) < length(input)
    for (i,si) in enumerate(input[2:end])
      if i in keys(mapped)
        continue
      end
      doesOverlap, rot, or, pos = findOverlap(allPoints, si)
      if doesOverlap
        si = [changeRefFrame(x, rot, or, pos) for x in si]
        push!(allPoints, si...)
        mapped[i] = (pos, rot, or)
      end
    end
  end
  #part1
  println(length(allPoints))
  #part2
  maxdiff = 0
  for (p,_,_) in values(mapped)
    for (q,_,_) in values(mapped)
      maxdiff = max(maxdiff, abs.(p-q) |> sum)
    end
  end
  println(maxdiff)
end

function main()
  input = getInput()
  part12(input)
end

main()
