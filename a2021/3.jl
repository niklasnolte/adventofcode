import Pipe:@pipe

function parseStr(S::String)::Array{Bool}
  return parse.(Bool, split(S,""))
end

function get_input()::Array{Bool,2}
  @pipe readlines("input3.txt") .|> parseStr |> reduce(hcat, _) |> _'
end

btoi(b) = @pipe convert.(Int8, b) |> parse(Int, string(_...), base=2)

function part1(input::Array{Bool,2})
  gamma = sum(input, dims=1)/size(input, 1) .>= 0.5
  delta = (gamma .- 1) .* -1
  g,d = [gamma, delta] .|> btoi
  println(g*d)
end

function most_common(l)
  return @pipe sum(l) / length(l) |> (_>=.5) |> convert(Int8, _)
end

function least_common(l)
  return @pipe sum(l) / length(l) |> (_<.5) |> convert(Int8, _)
end

function extract_rating(ratings, policy)
  cidx = 1
  while size(ratings, 1) > 1
    bit = policy(ratings[:,cidx])
    mask = ratings[:,cidx] .== bit
    ratings = ratings[mask,:]
    cidx += 1
  end
  return ratings |> btoi
end

function part2(input::Array{Bool,2})
  oxy = extract_rating(input, most_common)
  co2 = extract_rating(input, least_common)
  println(oxy*co2)
end

function main()
  input = get_input()
  part1(input)
  part2(input)
end

main()
