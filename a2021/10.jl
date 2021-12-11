import Statistics:median

function getClosing(o::Char)::Char
  return Dict('(' => ')', '[' =>']', '{'=> '}', '<'=> '>')[o]
end

function score(c::Char)::Int32
  return Dict(')' => 3, ']' => 57, '}'=> 1197, '>'=> 25137)[c]
end

function extendscore(c::Char)::Int32
  return Dict(')' => 1, ']' => 2, '}'=> 3, '>'=> 4)[c]
end

function scoreSequence(chars::Vector{Char})::Tuple{Int32, Int64} # (corrupt score, incomplete score)
  stack = []
  for c in chars
    if c in "([{<"
      append!(stack, c)
    else
      x = pop!(stack)
      if c != getClosing(x)
        return (score(c),0)
      end
    end
  end
  return (0,stack |> reverse .|> getClosing .|> extendscore |> addUp)
end

function addUp(x::Vector{Int32})::Int64
  s = 0
  for xi in x
    s *= 5
    s += xi
  end
  return s
end

removeZeros(x) = filter(i -> i > 0, x)
toInt(x) = convert(Int64, x)

function part1(x)
  scores = x .|> collect .|> scoreSequence
  getfield.(scores, 1) |> sum |> println
end

function part2(x)
  scores = x .|> collect .|> scoreSequence
  getfield.(scores, 2) |> removeZeros |> median |> toInt |> println
end

function main()
  input = readlines("input10.txt")
  part1(input)
  part2(input)
end

main()
