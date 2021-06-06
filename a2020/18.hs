import Safe(readMay)
import Control.Exception (assert)

data Op = Plus | Times deriving (Show, Eq)
data Tree op val =
       Leaf val |
       Node op (Tree op val) (Tree op val)
       deriving (Show)

testInput = "1 + 2 * 3 + 4 * 5 + 6"
testInput2 = "1 + (2 * 3) + (4 * (5 + 6))"


type PrecedenceComparison = 
  Maybe (Op, Int, Int) -- current best
  -> (Op, Int, Int) -- new
  -> Maybe (Op, Int, Int) -- the one with less precedence

-- choosing the operation with less precedence to
-- determine the root
-- Part1
choosePrecedenceP1 :: PrecedenceComparison
choosePrecedenceP1 best current@(_,_,nparan) =
  case best of
    Nothing -> Just current
    Just b@(_,_,bestParan) ->
      if nparan <= bestParan
        then Just current
        else Just b

-- Part2
choosePrecedenceP2 :: PrecedenceComparison
choosePrecedenceP2 best current@(op,_,nparan) =
  case best of
    Nothing -> Just current
    Just b@(bop,_,bestParan) ->
      case compare bestParan nparan of
      LT -> Just b
      GT -> Just current
      EQ ->
        if op == Times && bop == Plus
        then Just current
        else Just b

findRoot :: PrecedenceComparison -> String -> Maybe (Int, Op)
findRoot precedenceF str = do
  (op, idx, _) <- snd $ foldl search (0,Nothing) $ zip [0..] str
  return (idx, op)
  where
    search :: (Int, Maybe (Op, Int, Int))
              -> (Int, Char)
              -> (Int, Maybe (Op, Int, Int))
    search (nparan,best) (i,c) = case c of
      '*' -> (nparan, updateBest Times)
      '+' -> (nparan, updateBest Plus)
      '(' -> (nparan+1, best)
      ')' -> (nparan-1, best)
      _ -> (nparan, best)
      where
        updateBest op = precedenceF best (op,i,nparan)


evalValue :: String -> Maybe Int
evalValue x =
  let stripped = filter (\x -> x /= '(' && x /= ')') x in
  readMay stripped

buildTree :: PrecedenceComparison -> String -> Maybe (Tree Op Int)
buildTree precedenceF str =
  case evalValue str of
    (Just x) -> Just $ Leaf x
    Nothing -> do
      (idx, rootOp) <- findRoot precedenceF str
      let (lhs, rhs) = splitAt idx str
      l <- build lhs
      r <- build $ tail rhs
      return $ Node rootOp r l
  where build = buildTree precedenceF

evalTree :: Tree Op Int -> Int
evalTree t = case t of
  Leaf x -> x
  (Node op l r) ->
    let lhs = evalTree l in
    let rhs = evalTree r in
    case op of
    Times -> lhs * rhs
    Plus -> lhs + rhs

partX :: PrecedenceComparison -> String -> Maybe Int
partX predF input =
  let inp = lines $ filter (/=' ') input in
  sum <$> mapM (fmap evalTree . buildTree predF) inp

part1 = partX choosePrecedenceP1
part2 = partX choosePrecedenceP2

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Just 71) $ putStrLn "part1 good"
  assert (part1 testInput2 == Just 51) $ putStrLn "part1 good"

testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Just 231) $ putStrLn "part2 good"
  assert (part2 testInput2 == Just 51) $ putStrLn "part2 good"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
