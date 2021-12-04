import Pipe: @pipe
import Infiltrator: @infiltrate

function parseBingoLine(input::String)::Vector{Int8}
    nums = @pipe split(input, " ") |> _[_.!=""]
    return parse.(Int8, nums)
end

function parseBoards(input::Vector{String})::Array{Int8,3}
    board_mask = input .!= ""
    boards = @pipe input[board_mask] .|> parseBingoLine |> vcat(_...)
    boards = reshape(boards, 5, 5, :)
    return boards
end

parseRandomNumbers(x::String) = @pipe split(x, ",") .|> parse(Int8, _)

function hasWon(tags::BitMatrix)::Bool
    return (5 in sum(tags, dims = 1)) || (5 in sum(tags, dims = 2))
end

function runGameUntil1Wins(boards::Array{Int8,3}, numbers::Vector{Int8})::Int
    tagged_nums = falses(size(boards))
    for n in numbers
        tagged_nums .|= (boards .== n)
        for i = 1:size(boards, 3)
            tn = tagged_nums[:, :, i]
            if hasWon(tn)
                return n * sum(boards[:, :, i][.!tn])
            end
        end
    end
    throw(DomainError("No winner"))
end

function runGameUntilEveryoneWins(boards::Array{Int8,3}, numbers::Vector{Int8})::Int
    tagged_nums = falses(size(boards))
    who_lost_last = []
    for n in numbers
        tagged_nums .|= (boards .== n)
        who_lost = []
        for i = 1:size(boards, 3)
            if !hasWon(tagged_nums[:, :, i])
                who_lost = append!(who_lost, i)
            end
        end
        if length(who_lost) == 0 && length(who_lost_last) == 1
            winner = who_lost_last[1]
            return n * sum(boards[:, :, winner][.!tagged_nums[:, :, winner]])
        else
            who_lost_last = who_lost
        end
    end
    throw(DomainError("No winner"))
end

function part1(input::Vector{String})
    numbers = parseRandomNumbers(input[1])
    boards = parseBoards(input[2:end])
    println(runGameUntil1Wins(boards, numbers))
end

function part2(input::Vector{String})
    numbers = parseRandomNumbers(input[1])
    boards = parseBoards(input[2:end])
    println(runGameUntilEveryoneWins(boards, numbers))
end

function main()
    input = readlines("input4.txt")
    part1(input)
    part2(input)
end

main()
