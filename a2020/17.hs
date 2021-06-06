import qualified Data.Map as MAP
import Control.Exception (assert)

data Object = Cube | Empty deriving (Eq, Show)

toObj :: Char -> Either String Object
toObj '#' = Right Cube
toObj '.' = Right Empty
toObj x = Left $ "cannot parse " ++ show x

type Point = [Int] -- n dimensional hack
type Space = MAP.Map Point Object

testInput = ".#.\n..#\n###"
{-testInput = "#"-}


parseInput :: Int -> String -> Either String Space
parseInput nd str =
  let inp = lines str in
  let restD = replicate (nd-2) 0 in
  let map =
        [makeEntry ([i,j] ++ restD, toObj x)
          | (inpi,i) <- zip inp [0..],
            (x,j) <- zip inpi [0..]] in
  MAP.fromList <$> sequence map
  where
    makeEntry (p,o) = do
      obj <- o
      return (p,obj)

neighbors :: Point -> [Point]
neighbors = tail . neighborsImpl
  where
  neighborsImpl [] = [[]]
  neighborsImpl (xi0:rest) = do
    xi <- [0,-1, 1]
    r <- neighborsImpl rest
    return $ (xi+xi0):r

countNeighbors :: Space -> Point -> Int
countNeighbors s p =
  let nextObjs = map (`MAP.lookup` s) $ neighbors p in
  length $ filter (==Just Cube) nextObjs

makeEmptyNeighborhood :: Point -> Space
makeEmptyNeighborhood p =
  MAP.fromList $ zip (neighbors p) $ repeat Empty

evolveOne :: Space -> Point -> Object -> Object
evolveOne s p o =
  case countNeighbors s p of
    2 -> o
    3 -> Cube
    _ -> Empty

adjustEmptyEntries :: Space -> Space
adjustEmptyEntries =
  addEmptyNeighbors . removeEmptyEntries
  where
    removeEmptyEntries = MAP.filter (== Cube)
    addEmptyNeighbors s = foldl upd s $ MAP.keys s
    upd :: Space -> Point -> Space
    upd s p = MAP.union s $ makeEmptyNeighborhood p

evolve :: Space -> Space
evolve space =
  let space' = adjustEmptyEntries space in
  MAP.mapWithKey (evolveOne space) space'

evolveN :: Int -> Space -> Space
evolveN 0 = id
evolveN n = evolveN (n-1) . evolve


partX :: Int ->String -> Either String Int
partX nd inp = do
  m <- parseInput nd inp
  return $ length $ MAP.filter (==Cube) $ evolveN 6 m

part1 = partX 3
part2 = partX 4

testPart1 :: IO ()
testPart1 =
  assert (part1 testInput == Right 112) $ putStrLn "part1 worked"

testPart2 :: IO ()
testPart2 =
  assert (part2 testInput == Right 848) $ putStrLn "part2 worked"


main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
