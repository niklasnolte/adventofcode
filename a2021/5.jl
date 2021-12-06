import Pipe:@pipe

struct Point
    x::Int32
    y::Int32
end

const Line = Tuple{Point,Point}

function parseLine(ps::Vector{<:AbstractString})::Line
    parsePoint(p) = @pipe split(p, ",") .|> parse(Int32, _)
    return @pipe parsePoint.(ps) .|> Point(_...) |> Tuple
end

function parselines(lines::Vector{String})::Vector{Line}
    return split.(lines, " -> ") .|> parseLine
end

range_(p,q) = p:((p <= q) * 2 - 1):q

function getAllIntermediates(line::Line, useDiag::Bool)::Vector{Point}
  p,q = line
  rx = range_(p.x, q.x)
  ry = range_(p.y, q.y)
  if p.x == q.x || p.y == q.y
    return [Point(i,j) for i in rx for j in ry]
  elseif useDiag
    return [Point(i,j) for (i,j) in zip(rx, ry)]
  else
    return []
  end
end

function isHVLine(line::Line)::Bool
  p,q = line
  return p.x == q.x || p.y == q.y
end

function countDupls(points::Vector{Point})::Int32
  return @pipe unique(points) .|> (count(x -> x==_, points) > 1) |> count
end


function partX(lines::Vector{Line}, useDiag::Bool)
  points = @pipe getAllIntermediates.(lines, useDiag) |> vcat(_...)
  countDupls(points) |> println
end

part1(inp) = partX(inp, false)
part2(inp) = partX(inp, true)

function main()
    lines = readlines("input5.txt")
    lines = parselines(lines)
    part1(lines)
    part2(lines)
end

main()
