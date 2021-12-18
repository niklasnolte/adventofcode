struct Num
  val_::Union{Int64,Tuple{Num,Num}}
end
val(n::Num) = n.val_

getInput() = readlines("input18.txt")

function splitNum(x::String)::Tuple{Bool, String}
  greater9 = r"\d\d+"
  m = match(greater9, x)
  if m !== nothing
    val = parse(Int32, m.match)
    l = floor(val/2) |> Integer
    r = ceil(val/2) |> Integer
    subst = "[" * string(l) * "," * string(r) * "]"
    x = x[1:m.offset-1] * subst * x[m.offset+length(m.match):end]
    return (true, x)
  else
    return (false, x)
  end
end

function explodeNum(x::String)::Tuple{Bool, String}
  # find exploder
  scope = 0
  idx = 0
  for (i,xi) in enumerate(x)
    if xi == '['
      scope = scope + 1
      if scope > 4
        idx = i
        break
      end
    elseif xi == ']'
      scope = scope - 1
    end
  end
  if idx == 0
    return (false, x)
  else
    until = findfirst(==(']'), x[idx:end]) - 1
    replacethis = x[idx:idx+until]
    vals = split(replacethis[2:end-1], ",")
    l,r = parse.(Int32, vals)
    x = x[1:idx-1] * "0" * x[idx+until+1:end]
    #replace right
    for ridx in (idx+1):length(x)
      if isdigit(x[ridx])
        until = ridx + 1
        while isdigit(x[until])
          until += 1
        end
        rv = parse(Int32, x[ridx:until-1])
        x = x[1:ridx-1] * string(rv + r) * x[until:end] 
        break
      else
        ridx -= 1
      end
    end
    #replace left
    for lidx in (idx-1):-1:1
      if isdigit(x[lidx])
        until = lidx - 1
        while isdigit(x[until])
          until -= 1
        end
        lv = parse(Int32, x[until+1:lidx])
        x = x[1:until] * string(lv + l) * x[lidx+1:end] 
        break
      else
        lidx -= 1
      end
    end
    return (true, x)
  end
end

function reduceNum(x::String)::String
  while true
    exploded, x = explodeNum(x)
    if !exploded
      break
    end
  end
  split, x = splitNum(x)
  if !split
    return x
  else
    return reduceNum(x)
  end
end

function add(x::String, y::String)::String
  return reduceNum("["*x*","*y*"]")
end

function magnitude(x::Num)::Int64
  v = val(x)
  if v isa Int64
    return v
  else
    i, j = v
    return 3 * magnitude(i) + 2 * magnitude(j)
  end
end

function parseNum(line::AbstractString)::Num
  if all(isdigit.(collect(line)))
    return Num(parse(Int64, line))
  end
  scope = 0
  for (i, el) in enumerate(line)
    if scope == 1 && el == ','
      return Num((parseNum(line[2:i-1]), parseNum(line[i+1:end-1])))
    elseif el == '['
      scope += 1
    elseif el == ']'
      scope -= 1
    end
  end
  throw("wtf " * line)
end

function part1(input::Vector{String})
  x = reduce(add, input)
  parseNum(x) |> magnitude |> println
end

function part2(input::Vector{String})
  max_ = 0
  for (i,x) in enumerate(input)
    for y in input[i+1:end]
      cand = add(x,y) |> parseNum |> magnitude
      max_ = max(cand, max_)
      cand = add(y,x) |> parseNum |> magnitude
      max_ = max(cand, max_)
    end
  end
  println(max_)
end

function main()
  input = getInput()
  part1(input)
  part2(input)
end

main()
