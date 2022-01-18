function parseInstrs()
  instrs = readlines("input24.txt")
  divs = Vector{Bool}()
  n = Vector{Int64}()
  m = Vector{Int64}()
  for (i, instr) in enumerate(instrs)
    instrt, vals... = split(instr)
    if instrt == "div" && vals[1] == "z"
      # divide by one or 26
      push!(divs, vals[2] == "26")
    elseif instrt == "add" && vals[1] == "x"
      if isdigit(vals[2][1]) || vals[2][1] == '-'
        push!(n, parse(Int64, vals[2]))
      end
    elseif instr == "add y w"
      push!(m, parse(Int64, split(instrs[i+1])[end]))
    end
  end
  return (divs, n, m)
end

function solve(ns, ms, divs, solve_for_biggest = true)
  z = Vector{Int64}()
  # find pairs that need to match up
  pairs = Vector{Tuple{Int64,Int64}}()
  for (i, d) in enumerate(divs)
    if d
      push!(pairs, (pop!(z), i))
    else
      push!(z, i)
    end
  end
  inp = repeat([0], 14)
  # match them up
  # find inps such that
  # inp[r] - inp[l] = m[l] + n[r]
  for (l, r) in pairs
    diff = ms[l] + ns[r]
    if solve_for_biggest
      # start with big numbers for l candidate
      # this yields the biggest overall number
      range = 9:-1:1
    else
      range = 1:9
    end

    for candl in range
      candr = candl + diff
      if candr > 0 && candr < 10
        inp[l] = candl
        inp[r] = candr
        break
      end
    end
  end
  return inp
end

function part1(nums::Tuple{Vector{Bool},Vector{Int64},Vector{Int64}})
  divs, ns, ms = nums
  winner = solve(ns, ms, divs, true)
  join(winner) |> println
end

function part2(nums::Tuple{Vector{Bool},Vector{Int64},Vector{Int64}})
  divs, ns, ms = nums
  winner = solve(ns, ms, divs, false)
  join(winner) |> println
end

function main()
  nums = parseInstrs()
  part1(nums)
  part2(nums)
end

main()

