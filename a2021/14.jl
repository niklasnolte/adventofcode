function getInput()
  lines = readlines("input14.txt")
  seq = lines[1]
  rules = Dict(split.(lines[3:end], " -> "))
  return (seq, rules)
end

function getMaxMinVal(d, first, last)
  letters = Dict{Char,Int64}()

  for (k, v) in pairs(d)
    for ki in k
      letters[ki] = get(letters, ki, 0.0) + v
    end
  end
  for x in keys(letters)
    letters[x] = letters[x] / 2 + (x in (first, last))
  end
  vals = values(letters)
  return maximum(vals) - minimum(vals)
end

function partX(seq, rules, n)
  d = Dict{String,Int64}()
  for i = 1:(length(seq)-1)
    item = string(seq[i:i+1])
    d[item] = get(d, item, 0) + 1
  end

  for _ = 1:n
    dold = copy(d)
    for comb in keys(dold)
      new = rules[comb]
      left = string(comb[1], new)
      right = string(new, comb[2])
      d[left] = get(d, left, 0) + dold[comb]
      d[right] = get(d, right, 0) + dold[comb]
      d[comb] -= dold[comb]
    end
  end

  getMaxMinVal(d, seq[1], seq[end]) |> println
end

part1(seq, rules) = partX(seq, rules, 10)
part2(seq, rules) = partX(seq, rules, 40)

function main()
  seq, rules = getInput()
  part1(seq, rules)
  part2(seq, rules)
end

main()
