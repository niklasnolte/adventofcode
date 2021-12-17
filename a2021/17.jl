function getInput()
  inp = read("input17.txt", String)
  x, y = split(inp)[3:end]
  x = split(x[3:end-1], "..")
  y = split(y[3:end], "..")
  x = parse.(Int32, x) |> Tuple
  y = parse.(Int32, y) |> Tuple
  return x, y
end

inTarget(x, y, xr, yr)::Bool =
  x >= xr[1] && x <= xr[2] && y >= yr[1] && y <= yr[2]

# if lower than yr or "righter" than xr, we have a problem
failed(x, y, xr, yr)::Bool = x > xr[2] || y < yr[1]

function part1(_, yr)
  if yr[1] > 0
    besty = yr[2]
  elseif yr[2] < 0
    besty = -yr[1] - 1
  else # 0 in yr
    throw("infinity is the right answer here")
  end
  div(besty * (besty + 1), 2) |> println
end

function part2(xr, yr)
  # assume xr[1] > 0
  # assume yr[2] < 0
  
  nvalid = 0
  minvx = ceil(-.5 + sqrt(0.25 + 2 * xr[1]))
  maxvx = xr[2]
  minvy = yr[1]
  maxvy = -yr[1] - 1
  println((maxvx - minvx) * (maxvy - minvy))

  for vx0 = minvx:maxvx
    for vy0 = minvy:maxvy
      vx = vx0
      vy = vy0
      x = 0
      y = 0
      while !failed(x, y, xr, yr)
        x += vx
        y += vy
        vx -= sign(vx)
        vy -= 1
        if inTarget(x, y, xr, yr)
          nvalid += 1
          break
        end
      end
    end
  end
  println(nvalid)
end

function main()
  xr, yr = getInput()
  part1(xr, yr)
  part2(xr, yr)
end

main()
