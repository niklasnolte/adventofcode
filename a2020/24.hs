import Control.Exception (assert)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Debug.Trace(trace)

type Coordinate = (Int, Int)

type HexGrid =
  M.Map
    Coordinate
    Bool -- flipped or not

data Dir = SW | W | NW | NE | E | SE deriving (Enum, Bounded, Show)

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

move :: Coordinate -> Dir -> Coordinate
-- from https://www.redblobgames.com/grids/hexagons/ (even-r horizontal layout)
move (x, y) d = case d of
  SW -> (if even y then x else x -1, y + 1)
  W -> (x -1, y)
  NW -> (if even y then x else x -1, y -1)
  NE -> (if odd y then x else x + 1, y -1)
  E -> (x + 1, y)
  SE -> (if odd y then x else x + 1, y + 1)

inputFileP :: Parser [[Dir]]
inputFileP = dirsP `endBy` newline <* eof

dirsP :: Parser [Dir]
dirsP = many dirP

toDir :: [Char] -> Dir
toDir "sw" = SW
toDir "w" = W
toDir "nw" = NW
toDir "ne" = NE
toDir "e" = E
toDir "se" = SE
toDir x = error $ "can not convert " ++ x ++ " to valid direction"

dirP :: Parser Dir
dirP = toDir <$> choice (map (try . string) ["sw", "w", "nw", "ne", "e", "se"])

dirListToCoord :: [Dir] -> Coordinate
dirListToCoord = foldl move (0, 0)

flipTile :: HexGrid -> Coordinate -> HexGrid
flipTile g target = case M.lookup target g of
  (Just True) -> M.delete target g
  (Just False) -> M.insert target True g
  Nothing -> M.insert target True g

testInput =
  unlines
    [ "sesenwnenenewseeswwswswwnenewsewsw",
      "neeenesenwnwwswnenewnwwsewnenwseswesw",
      "seswneswswsenwwnwse",
      "nwnwneseeswswnenewneswwnewseswneseene",
      "swweswneswnenwsewnwneneseenw",
      "eesenwseswswnenwswnwnwsewwnwsene",
      "sewnenenenesenwsewnenwwwse",
      "wenwwweseeeweswwwnwwe",
      "wsweesenenewnwwnwsenewsenwwsesesenwne",
      "neeswseenwwswnwswswnw",
      "nenwswwsewswnenenewsenwsenwnesesenew",
      "enewnwewneswsewnwswenweswnenwsenwsw",
      "sweneswneswneneenwnewenewwneswswnese",
      "swwesenesewenwneswnwwneseswwne",
      "enesenwswwswneneswsenwnewswseenwsese",
      "wnwnesenesenenwwnenwsewesewsesesew",
      "nenewswnwewswnenesenwnesewesw",
      "eneswnwswnwsenenwnwnwwseeswneewsenese",
      "neswnwewnwnwseenwseesewsenwsweewe",
      "wseweeenwnesenwwwswnew"
    ]

flipOnGrid :: [[Dir]] -> HexGrid
flipOnGrid ds =
  foldl flipTile mempty $ dirListToCoord <$> ds

part1 :: String -> Either ParseError Int
part1 inp = do
  toBeFlipped <- parse inputFileP "" inp
  return $ M.size $ flipOnGrid toBeFlipped

testPart1 :: IO ()
testPart1 = assert (part1 testInput == Right 10) putStrLn "part1 worked"

getNeighbors :: Coordinate -> [Coordinate]
getNeighbors c = map (move c) [SW ..]

countNeighbors :: HexGrid -> Coordinate -> Int
countNeighbors g c = sum $ do
  neighbor <- getNeighbors c
  case M.lookup neighbor g of
    (Just True) -> [1]
    (Just False) -> [0]
    Nothing -> [0]

applyRules :: HexGrid -> HexGrid
applyRules g = foldl flipTile g flipThese
  where
    nCounts = M.mapWithKey (\k a -> (a, countNeighbors g k)) g
    flipThese = M.keys $ M.filter mustFlip nCounts
    mustFlip (flipped, nCount)
      | nCount == 2 && not flipped = True
      | flipped && (nCount == 0 || nCount > 2) = True
      | otherwise = False

removeWhites :: HexGrid -> HexGrid
-- sounds racist lol
removeWhites = M.filter (==True)

evolveGrid :: HexGrid -> HexGrid
evolveGrid g =
  removeWhites $ applyRules gWithWhites
  where
    -- lets be a bit lazy. first add "missing" neighbors explicitly to the map as white
    neighbors = S.fromList $ concat $ M.elems $ M.mapWithKey (\k _ -> getNeighbors k) g
    gWithWhites = foldl maybeInsertWhite g neighbors
    maybeInsertWhite :: HexGrid -> Coordinate -> HexGrid
    maybeInsertWhite g c = case M.lookup c g of
      (Just True) -> g
      (Just False) -> g
      Nothing -> M.insert c False g

part2 :: String -> Either ParseError Int
part2 inp = do
  toBeFlipped <- parse inputFileP "" inp
  let gridOnDay0 = flipOnGrid toBeFlipped
  return $ M.size $ applyN 100 evolveGrid gridOnDay0

testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Right 2208) putStrLn "part2 worked"

main :: IO ()
main = do
  testPart1
  input <- getContents
  print $ part1 input
  testPart2
  print $ part2 input
