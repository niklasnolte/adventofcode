Point = Tuple{Int64,Int64}
Map = Dict{Point,Bool}

function getInput()::Tuple{String,Map}
  input = read("input20.txt", String)
  alg, img = split(input, "\n\n")
  img = split(img, "\n")
  img_ = Map()
  for (i, xs) in enumerate(img)
    for (j, y) in enumerate(xs)
      if y == '#'
        img_[(i, j)] = true
      else
        img_[(i, j)] = false
      end
    end
  end
  return alg, img_
end

kernel((x, y)::Point)::Vector{Point} =
  [
    (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y),
    (x, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x + 1, y + 1)
  ]

function convolve(img::Map, alg::String, default::Bool)::Tuple{Map,Bool}
  next = copy(img)
  for (idx, _) in img
    for p in kernel(idx)
      vals = [get(img, n, default) for n in kernel(p)] .|> Int8
      algidx = parse(Int64, join(vals), base = 2) + 1
      next[p] = alg[algidx] == '#'
    end
  end
  nextdefault = alg[(default ? end : 1)] == '#'
  for (idx, v) in next
    if v == nextdefault
      delete!(next, idx)
    end
  end
  return (next, nextdefault)
end

function partX(alg, img, n)
  default = false
  for _ = 1:n
    img, default = convolve(img, alg, default)
  end
  sum(values(img)) |> println
end

part1(a,i) = partX(a,i,2)
part2(a,i) = partX(a,i,50)

function main()
  alg, img = getInput()
  part1(alg, img)
  part2(alg, img)
end

main()
