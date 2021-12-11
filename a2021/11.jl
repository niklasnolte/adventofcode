import Pipe: @pipe

function getInput()
  input = @pipe readlines("input11.txt") .|> collect .|> parse.(Int32, _)
  return reshape(vcat(input...), 10, 10)
end

inbounds(xy) = xy .|> (xi -> xi > 0 && xi < 11) |> all

function neighbors(idx)
  x, y = Tuple(idx)
  ns = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  return @pipe ns .|>
               (((i, j),) -> (x + i, y + j)) |>
               filter(inbounds, _)
end

function letItFlash(octs)
  flashys = findall(>(9), octs)
  while length(flashys) > 0
    octs[flashys] .= 0
    enlightened = vcat(neighbors.(flashys)...)
    for (i, j) in enlightened
      octs[i, j] += 1 * (octs[i, j] != 0)
    end
    flashys = filter(((i, j),) -> octs[i, j] > 9,
                     Set(enlightened)) .|> CartesianIndex
  end
  return octs
end

function iterate(octs)
  octs = map(x -> x + 1, octs)
  return letItFlash(octs)
end

function part1(x)
  flashes = 0
  for _ in 1:100
    x = iterate(x)
    flashes += ((x .== 0) |> sum)
  end
  println(flashes)
end

function part2(x)
  it = 0
  while true
    it += 1
    x = iterate(x)
    if (x .== 0) |> all
      println(it)
      break
    end
  end
end

function main()
  getInput() |> part1
  getInput() |> part2
end

main()
