import DataStructures: PriorityQueue, dequeue!, peek

@enum Pod A B C D
Position = Tuple{Int64,Int64}
Board = Dict{Position,Pod}

letterToPod = Dict(
  'A' => A,
  'B' => B,
  'C' => C,
  'D' => D
)

stepCost = Dict(
  A => 1,
  B => 10,
  C => 100,
  D => 1000
)

allPositions = Set(
  [
  (0, 0),
  (1, 0),
  (3, 0),
  (5, 0),
  (7, 0),
  (9, 0),
  (10, 0),
  (2, 1),
  (2, 2),
  (4, 1),
  (4, 2),
  (6, 1),
  (6, 2),
  (8, 1),
  (8, 2),
]
)

bagFor = Dict(
  A => 2,
  B => 4,
  C => 6,
  D => 8
)

function getInput()::Board
  input = read("input23.txt", String)
  letters = filter(isuppercase, input)
  poss = (
    (2, 1),
    (4, 1),
    (6, 1),
    (8, 1),
    (2, 2),
    (4, 2),
    (6, 2),
    (8, 2),
  )
  return Dict(p => letterToPod[l] for (p, l) in zip(poss, letters))
end


function moveCost(pod::Pod, from::Position, to::Position)::Int64
  fx, fy = from
  tx, ty = to
  return (abs(ty - fy) + abs(tx - fx)) * stepCost[pod]
end

function canMove(from::Position, to::Position, board::Board)::Bool
  fx, fy = from
  tx, ty = to
  pod = board[from]
  if from == to
    return false
  elseif fy != 0
    if ty != 0
      return false
    elseif bagFor[pod] == fx # dont move out if already in place
      if get(board, (fx, 2), pod) == pod
        return false
      end
    end
  elseif fy == 0
    if ty == 0 # cannot move within hallway
      return false
    end
    # can only move into the right room
    if tx != bagFor[pod]
      return false
      # make sure to move fully down if possible
    elseif ty == 1 && (!((tx, 2) in keys(board)) || board[(tx, 2)] != pod)
      return false
    end
  end
  # path is clear?
  pods = keys(board)
  for y in 1:fy-1
    if (fx, y) in pods
      return false
    end
  end
  for y in 1:ty
    if (tx, y) in pods
      return false
    end
  end
  xi = fx
  while xi != tx
    xi += sign(tx - fx)
    if (xi, 0) in pods
      return false
    end
  end
  return true
end

function possibleMoves(pos::Position, board::Board)::Vector{Position}
  moves = Vector{Position}()
  for target in allPositions
    if canMove(pos, target, board)
      push!(moves, target)
    end
  end
  return moves
end

function manyWorlds(board::Board)::Vector{Tuple{Board,Int64}}
  outcomes = Vector{Tuple{Board,Int64}}()
  for (pos, pod) in board
    for newpos in possibleMoves(pos, board)
      nextpods = copy(board)
      delete!(nextpods, pos)
      nextpods[newpos] = pod
      push!(outcomes, (nextpods, moveCost(pod, pos, newpos)))
    end
  end
  return outcomes
end

function finished(board::Board)::Bool
  for ((x, y), pod) in board
    if y == 0
      return false
    end
    if x != bagFor[pod]
      return false
    end
  end
  return true
end

function part1(board::Board)
  boards = PriorityQueue(board => 0)
  seen = Dict(board => 0)
  while length(boards) > 0
    _, cost = peek(boards)
    board = dequeue!(boards)
    if finished(board)
      println(cost)
      break
    end
    for (nb, nc) in manyWorlds(board)
      newcost = cost + nc
      if get(seen, nb, 1e20) > newcost
        boards[nb] = newcost
        seen[nb] = newcost
      end
    end
  end
end

function main()
  board = getInput()
  part1(board)
end

main()
