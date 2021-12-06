function n_fish(state::Int32)
    if state >= 0
        return 1
    end
    states = [[1]; repeat([0], 8)]
    for i = 1:-state
        ripefish = states[1]
        for n = 1:8
            states[n] = states[n+1]
        end
        states[7] += ripefish
        states[9] = ripefish
    end
    return sum(values(states))
end

function partX(fish::Vector{Int32}, n_days::Int32)
    fishEvolved = fish .- n_days
    sum(n_fish.(fishEvolved)) |> println
end

part1(fish) = partX(fish, Int32(80))
part2(fish) = partX(fish, Int32(256))

function main()
    states = parse.(Int32, split(readlines("input6.txt")[1], ","))
    part1(states)
    part2(states)
end

main()
