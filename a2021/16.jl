abstract type AbstractPacket end
type(x::AbstractPacket) = x.type_
version(x::AbstractPacket) = x.version_
content(_::AbstractPacket) = throw("not implemented")

struct Literal <: AbstractPacket
  version_::Int8
  type_::Int8
  numbers_::Int64
end

content(x::Literal)::Int64 = x.numbers_

struct Operator <: AbstractPacket
  version_::Int8
  type_::Int8
  subpackets_::Vector{AbstractPacket}
end

content(x::Operator)::Vector{AbstractPacket} = x.subpackets_

HEX2BIN(x) = Dict(
  '0' => "0000",
  '1' => "0001",
  '2' => "0010",
  '3' => "0011",
  '4' => "0100",
  '5' => "0101",
  '6' => "0110",
  '7' => "0111",
  '8' => "1000",
  '9' => "1001",
  'A' => "1010",
  'B' => "1011",
  'C' => "1100",
  'D' => "1101",
  'E' => "1110",
  'F' => "1111"
)[x]

function getInput()::String
  x = read("input16.txt", String) |> strip |> collect
  return join(map(HEX2BIN, x), "")
end

bin(x) = parse(Int8, x, base=2)
bin(t, x) = parse(t, x, base=2)

function parseLiteralContent(p::AbstractString)::Tuple{AbstractString, Int64}
  nums = Vector{Char}()
  i = 1
  while true
    append!(nums,p[i+1:i+4] |> collect)
    i += 5
    if p[i-5] == '0'
      break
    end
  end
  return p[i:end], bin(Int64, join(nums))
end

function parseOperatorContent(enc::AbstractString)::Tuple{AbstractString, Vector{AbstractPacket}}
  I = enc[1] |> bin
  packets = Vector{AbstractPacket}()
  if I == 0
    len = bin(Int64, enc[2:16])
    enc = enc[17:end]
    i = 0
    while i < len
      (nenc, packet) = parsePacket(enc)
      push!(packets, packet)
      i += length(enc) - length(nenc)
      enc = nenc
    end
  elseif I == 1
    nSP = bin(Int64, enc[2:12])
    enc = enc[13:end]
    while length(packets) < nSP
      (enc, packet) = parsePacket(enc)
      push!(packets, packet)
    end
  end
  return enc, packets
end

function parsePacket(p::AbstractString)::Tuple{AbstractString, AbstractPacket}
  V = p[1:3] |> bin
  T = p[4:6] |> bin
  if T == 4
    p, val = parseLiteralContent(p[7:end])
    return p, Literal(V, T, val)
  else
    p, val = parseOperatorContent(p[7:end])
    return p, Operator(V, T, val)
  end
end

function sumVersionNumbers(p::AbstractPacket)::Int64
  if p isa Literal
    return version(p)
  elseif p isa Operator
    return version(p) + (sumVersionNumbers.(content(p)) |> sum)
  end
end

function part1(inp::String)
  _,p = parsePacket(inp)
  sumVersionNumbers(p) |> println
end

reduce_(op) = x -> reduce(op, x)

decodeValue(i::Int64)::Int64 = i
function decodeValue(p::AbstractPacket)::Int64
  t = type(p)
  c = content(p)
  ops = (
    sum,
    prod,
    minimum,
    maximum,
    identity,
    reduce_(>),
    reduce_(<),
    reduce_(==)
  )
  return c .|> decodeValue |> ops[t+1]
end

function part2(inp::String)
  _,p = parsePacket(inp)
  decodeValue(p) |> println
end

function main()
  input = getInput()
  part1(input)
  part2(input)
end

main()
