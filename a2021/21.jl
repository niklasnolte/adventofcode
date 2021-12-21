function getInput()
  p1, p2 = readlines("input21.txt")
  p1 = parse(Int32, split(p1)[end])
  p2 = parse(Int32, split(p2)[end])
  return p1, p2
end

function part1(p1, p2)
  #deterministic die
  nrolls = 0
  die = 0 # +1 always
  players = [p1 - 1, p2 - 1]
  scores = [0,0]
  idx = 0 # idx of player
  while maximum(scores) < 1000
    roll = 0
    for i in 1:3
      roll += die + 1
      die = (die + 1) % 100
      nrolls += 1
    end
    players[idx+1] = (players[idx+1] + roll) % 10
    scores[idx+1] += players[idx+1] + 1
    idx = abs(idx - 1)
  end
  println(nrolls * minimum(scores))
end

PlayerState = Tuple{Int32, Int32}
GameState = Tuple{PlayerState, PlayerState}

function advance((p, s)::PlayerState)::Vector{PlayerState}
  result = Vector{PlayerState}()
  for r1 in 1:3
    for r2 in 1:3
      for r3 in 1:3
        roll = r1 + r2 + r3
        newp = (p + roll) %10
        push!(result, (newp, s + newp + 1))
      end
    end
  end
  return result
end


function part2(p1, p2)
  games = Dict{GameState, Int64}() 
  games[((p1 - 1, 0), (p2 - 1, 0))] = 1

  #play until nothing changes

  change = true
  while change
    change = false
    # play player 1
    tmp = copy(games)
    for ((p1, p2),n) in games
      _,s1 = p1
      _,s2 = p2
      if s2 >= 21 || s1 >= 21
        continue
      end
      change = true
      p1ns = advance(p1)
      for p1n in p1ns
        tmp[(p1n, p2)] = get(tmp, (p1n, p2), 0) + n
      end
      tmp[(p1, p2)] -= n
      if tmp[(p1, p2)] == 0
        delete!(tmp, (p1, p2))
      end
    end 
    games = tmp
    # play player 2
    tmp = copy(games)
    for ((p1, p2),n) in games
      _,s1 = p1
      _,s2 = p2
      if s2 >= 21 || s1 >= 21
        continue
      end
      change = true
      p2ns = advance(p2)
      for p2n in p2ns
        tmp[(p1, p2n)] = get(tmp, (p1, p2n), 0) + n
      end
      tmp[(p1, p2)] -= n
      if tmp[(p1, p2)] == 0
        delete!(tmp, (p1, p2))
      end
    end 
    games = tmp
  end
  p1_wins = 0
  p2_wins = 0
  for (((p1, s1), (p2, s2)), n) in games
    if s1 >= 21
      p1_wins += n
    elseif s2 >= 21
      p2_wins += n
    end
  end
  println(p1_wins," ", p2_wins)
end

function main()
  p1, p2 = getInput()
  part1(p1, p2)
  part2(p1, p2)
end

main()
