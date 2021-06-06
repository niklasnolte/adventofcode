import Text.Read (readEither)
import Control.Exception (assert)

data Direction = North | East | South | West deriving (Enum, Eq, Show)
data InstrT = MoveNorth | MoveEast | MoveWest | MoveSouth | TurnLeft | TurnRight | MoveForward deriving (Eq, Show)
type Instr = (InstrT, Int)

type Point = (Int, Int)
type Ship1 = (Point, Direction)
type Ship2 = (Point, Point)

unitVector :: Direction -> Point
unitVector dir = case dir of
  North -> (-1,0)
  South -> (1,0)
  West -> (0,-1)
  East -> (0,1)

toInstrT :: Char -> Either String InstrT
toInstrT c = case c of
  'N' -> Right MoveNorth
  'E' -> Right MoveEast
  'W' -> Right MoveWest
  'S' -> Right MoveSouth
  'L' -> Right TurnLeft
  'R' -> Right TurnRight
  'F' -> Right MoveForward
  other -> Left $ "cannot parse " ++ show other

parseInput :: String -> Either String [Instr]
parseInput = mapM parseInstr . lines

parseInstr :: String -> Either String Instr
parseInstr (instrc:num) = do
  instrt <- toInstrT instrc
  n <- readEither num
  return (instrt, n)

rotateShip1 :: Instr -> Direction -> Direction
rotateShip1 (instrt, n) dir =
  let sign = case instrt of
        TurnLeft -> -1
        TurnRight -> 1
  in toEnum $ (fromEnum dir + sign * (n `div` 90)) `mod` 4

moveShip1 :: Ship1 -> Instr -> Ship1
moveShip1 ((x,y), dir) instr@(instrt,n) = case instrt of
  MoveNorth -> ((x-n, y), dir)
  MoveSouth -> ((x+n, y), dir)
  MoveWest -> ((x, y-n), dir)
  MoveEast -> ((x, y+n), dir)
  MoveForward ->
    let (dx, dy) = unitVector dir in
    ((x+n*dx, y+n*dy), dir)
  _ -> ((x,y), rotateShip1 instr dir)


testInput = "F10\nN3\nF7\nR90\nF11"

part1 :: String -> Either String Int
part1 inp = do
  input <- parseInput inp
  let initShip = ((0,0), East)
  let ((xf, yf), _) = foldl moveShip1 initShip input
  return $ abs xf + abs yf

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Right 25) $ print "part1 worked"

rotateWayPoint :: Point -> Instr -> Point
rotateWayPoint (x,y) (instrt, deg) = case deg of
  0 -> (x,y)
  _ -> case instrt of
    TurnLeft -> rotateWayPoint (-y,x) (instrt, deg-90)
    TurnRight -> rotateWayPoint (y,-x) (instrt, deg-90)

moveShip2 :: Ship2 -> Instr -> Ship2
moveShip2 ((x,y), wp@(wx, wy)) instr@(instrt, n) = case instrt of
  MoveNorth -> ((x,y), (wx-n, wy))
  MoveSouth -> ((x,y), (wx+n, wy))
  MoveWest -> ((x,y), (wx, wy-n))
  MoveEast -> ((x,y), (wx, wy+n))
  MoveForward -> ((x+n*wx, y+n*wy), (wx, wy))
  _ -> ((x,y), rotateWayPoint wp instr)

part2 :: String -> Either String Int
part2 inp = do
  input <- parseInput inp
  let initShip = ((0,0), (-1,10))
  let ((xf, yf), _) = foldl moveShip2 initShip input
  return $ abs xf + abs yf

testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Right 286) $ print "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
