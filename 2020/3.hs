import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Control.Exception (assert)
import Data.Maybe (fromJust)

type Key = (Int, Int)

data ForestObj = Space | Tree deriving (Show, Bounded, Eq)

data Forest = Forest {
                getHeight :: Int,
                getWidth :: Int,
                getMap ::  M.Map Key ForestObj
              } deriving (Show, Eq)

toForestObj :: Char -> Maybe ForestObj
toForestObj '#' = Just Tree
toForestObj '.' = Just Space
toForestObj _ = Nothing

forestLookup :: Key -> Forest -> Maybe ForestObj
forestLookup (x,y) (Forest _ width m) = M.lookup (x, y `mod` width) m

mapFrom2DList :: [[ForestObj]] -> Forest
mapFrom2DList [] = Forest 0 0 mempty
mapFrom2DList xs@(x:_) =
  let height = length xs in
  let width = length x in
  let heights = [0..height-1] in
  let widths = [0..width-1] in
  let grid = (,) <$> heights <*> widths in
  Forest height width $ M.fromList $ zip grid (concat xs)

inputFileP :: GenParser Char st Forest
inputFileP = do
  objLines <- lineP `endBy` char '\n'
  return $ mapFrom2DList objLines

lineP :: GenParser Char st [ForestObj]
lineP = do
  s <- many (oneOf "#.")
  return $ map (fromJust . toForestObj) s

testParse :: IO ()
testParse =
  let testIn = unlines [".#", ".."] in
  let result = Forest {
                 getWidth = 2,
                 getHeight = 2,
                 getMap = M.fromList [((0,0), Space),
                                      ((0,1), Tree),
                                      ((1,0), Space),
                                      ((1,1), Space)]
               } in
  let (Right parsed) = parse inputFileP "unknown" testIn in
  assert (parsed == result) $ return ()


type PathPolicy = (Int -> Int, Int -> Int)

followPathWithPolicy :: PathPolicy -> Key -> Forest -> Maybe [ForestObj]
followPathWithPolicy (fx, fy) k f = reverse <$> follow k f where
  follow :: Key -> Forest -> Maybe [ForestObj]
  follow k@(x,y) forest =
    if x >= getHeight forest
    then Just []
    else do
      obj <- forestLookup k forest
      rest <- follow (fx x, fy y) forest
      return $ obj:rest


parseInputToForest :: String -> Maybe Forest
parseInputToForest input =
  case parse inputFileP "unknown" input of
     (Left _) -> Nothing
     (Right x) -> Just x

countTreesOnPathWithPolicy :: Forest -> PathPolicy -> Maybe Int
countTreesOnPathWithPolicy f policy = do
  path <- followPathWithPolicy policy (0,0) f
  return $ length $ filter (== Tree) path

part1 :: String -> Maybe Int
part1 input = do
  forest <- parseInputToForest input
  countTreesOnPathWithPolicy forest ((+1), (+3))

testInput = unlines [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"]

testPart1 :: IO ()
testPart1 =
  assert (Just 7 == part1 testInput) return ()

part2 :: String -> Maybe Int
part2 input = do
  forest <- parseInputToForest input
  product <$> mapM (countTreesOnPathWithPolicy forest)
    [((+1), (+1)),
     ((+1), (+3)),
     ((+1), (+5)),
     ((+1), (+7)),
     ((+2), (+1))]

testPart2 :: IO ()
testPart2 = assert (Just 336 == part2 testInput) return ()

main = do
  testParse
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
