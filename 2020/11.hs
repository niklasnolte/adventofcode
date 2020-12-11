import qualified Data.HashMap.Strict as Map
import Control.Exception (assert)

testInput = ["L.LL.LL.LL",
             "LLLLLLL.LL",
             "L.L.L..L..",
             "LLLL.LL.LL",
             "L.LL.LL.LL",
             "L.LLLLL.LL",
             "..L.L.....",
             "LLLLLLLLLL",
             "L.LLLLLL.L",
             "L.LLLLL.LL"]

testInputAfter5Iterations = ["#.#L.L#.##",
                             "#LLL#LL.L#",
                             "L.#.L..#..",
                             "#L##.##.L#",
                             "#.#L.LL.LL",
                             "#.#L#L#.##",
                             "..L.L.....",
                             "#L#L##L#L#",
                             "#.LLLLLL.L",
                             "#.#L#L#.##"]

data Object = EmptySpace | EmptySeat | OccupiedSeat deriving (Eq, Show)

toObject :: Char -> Either String Object
toObject c = case c of
  '#' -> Right OccupiedSeat
  'L' -> Right EmptySeat
  '.' -> Right EmptySpace
  c -> Left $ "cannot parse " ++ show c

type Position = (Int, Int)
type Direction = (Int, Int)
type SeatConf = Map.HashMap Position Object

parseInput :: String -> Either String SeatConf
parseInput inp = do
  let inp' = lines inp
  let height = length inp'
  let width = length $ takeWhile (/='\n') inp
  let points = [(a,b) | a <- [0..height-1], b <- [0..width-1]]
  objs <- mapM toObject $ foldl1 (++) inp'
  return $ Map.fromList $ zip points objs

type CountingFunT = Position -> SeatConf -> Int

countAdjacentOccupied :: CountingFunT
countAdjacentOccupied (x,y) sc =
  length $ filter (==OccupiedSeat) adj
  where
    getObjAt x = Map.lookupDefault EmptySpace x sc
    adj = map getObjAt adjPoss
    adjPoss = tail [(i,j) | i <- [x, x-1, x+1],
                            j <- [y, y-1, y+1]]

countVisibleOccupied :: CountingFunT
countVisibleOccupied (x,y) sc = nvisOcc
  where
    getObjAt p = Map.lookup p sc
    nvisOcc = sum $ tail [extendBounded (dx,dy)
                          | dx <- [0, -1, 1],
                            dy <- [0,-1,1]]
    extendBounded :: Direction -> Int
    extendBounded (dx,dy) =
      case getObjAt (x+dx, y+dy) of
        Nothing -> 0
        (Just EmptySeat) -> 0
        (Just OccupiedSeat) -> 1
        (Just EmptySpace) -> extendBounded (dx+signum dx, dy+signum dy)

evolveWithPolicy :: Int ->
                    CountingFunT ->
                    SeatConf ->
                    SeatConf
evolveWithPolicy tolerance countingFun sc =
  Map.mapWithKey evolveSeat sc
  where
    evolveSeat :: Position -> Object -> Object
    evolveSeat pos obj = case obj of
      EmptySpace -> EmptySpace
      EmptySeat ->
        if countingFun pos sc == 0
        then OccupiedSeat
        else EmptySeat
      OccupiedSeat ->
        if countingFun pos sc > tolerance
        then EmptySeat
        else OccupiedSeat

evolveUntilStableWithPolicy :: Int -> CountingFunT -> SeatConf -> SeatConf
evolveUntilStableWithPolicy tol cf sc =
  let sc' = evolveWithPolicy tol cf sc in
  if sc' == sc
  then sc
  else evolveUntilStableWithPolicy tol cf sc'

testEvolve :: IO ()
testEvolve = do
  let after5 = do
      initSc <- parseInput $ unlines testInput
      return $ evolveP1 $ evolveP1 $ evolveP1 $ evolveP1 $ evolveP1 initSc
  assert (parseInput (unlines testInputAfter5Iterations) == after5) $
    putStrLn "evolving works"
  where evolveP1 = evolveWithPolicy 3 countAdjacentOccupied

countOccupied :: SeatConf -> Int
countOccupied = Map.size . Map.filter (== OccupiedSeat)

part1 :: String -> Either String Int
part1 sc =
  let evolve' = evolveUntilStableWithPolicy 3 countAdjacentOccupied in
  countOccupied . evolve' <$> parseInput sc

testPart1 :: IO ()
testPart1 = assert (part1 (unlines testInput) == Right 37)
              $ putStrLn "part1 works"

part2 :: String -> Either String Int
part2 sc =
  let evolve' = evolveUntilStableWithPolicy 4 countVisibleOccupied in
  countOccupied . evolve' <$> parseInput sc

testPart2 :: IO ()
testPart2 = assert (part2 (unlines testInput) == Right 26)
              $ putStrLn "part2 works"

main = do
  testEvolve
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
