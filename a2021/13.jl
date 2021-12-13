function getInput()
  input = readlines("input13.txt")
  splitidx = findfirst(==(""), input)
  dots = [parse.(Int32, split(x, ",")).+1 for x in input[1:splitidx - 1]]
  dims = (maximum([x[1] for x in dots]), maximum([x[2] for x in dots]))
  dots = dots .|> Tuple .|> CartesianIndex
  field = falses(dims)
  field[dots] .= 1
  folds = [split(x)[end] for x in input[splitidx + 1:end]]
  folds = [split(x, "=") for x in folds]
  folds = [(or, parse(Int32, v) + 1) for (or,v) in folds]
  return field,folds
end

function printMap(m)
  maxx = size(m, 1)
  p = [(x ? "#" : ".") for x in m]
  for (i,pi) in enumerate(p)
    print(pi)
    if i % maxx == 0
      print("\n")
    end
  end
end

function fold(map, instr)
  or, v = instr
  if or == "x"
    map[1:v-1, :] .|= map[end:-1:v+1,:]
    map = map[1:v-1, :]
  elseif or == "y"
    map[:, 1:v-1] .|= map[:,end:-1:v+1]
    map = map[:, 1:v-1]
  end
  return map
end

function part1(dots, folds)
  fold(dots, folds[1]) |> sum |> println
end

function part2(dots, folds)
  # why do i have to do that?
  dots = hcat(dots,falses(size(dots, 1)))

  for f in folds
    dots = fold(dots, f)
  end
  printMap(dots)
end

function main()
  dots, folds = getInput()
  part1(dots, folds)
  part2(dots, folds)
end

main()
