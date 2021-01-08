import Text.ParserCombinators.Parsec
import qualified Data.Map as MAP
import Data.List (transpose, elemIndex, elemIndices, (\\))
import Control.Exception(assert)

testInput = unlines
  ["Tile 2311:",
  "..##.#..#.",
  "##..#.....",
  "#...##..#.",
  "####.#...#",
  "##.##.###.",
  "##...#.###",
  ".#.#.#..##",
  "..#....#..",
  "###...#.#.",
  "..###..###",
  "",
  "Tile 1951:",
  "#.##...##.",
  "#.####...#",
  ".....#..##",
  "#...######",
  ".##.#....#",
  ".###.#####",
  "###.##.##.",
  ".###....#.",
  "..#.#..#.#",
  "#...##.#..",
  "",
  "Tile 1171:",
  "####...##.",
  "#..##.#..#",
  "##.#..#.#.",
  ".###.####.",
  "..###.####",
  ".##....##.",
  ".#...####.",
  "#.##.####.",
  "####..#...",
  ".....##...",
  "",
  "Tile 1427:",
  "###.##.#..",
  ".#..#.##..",
  ".#.##.#..#",
  "#.#.#.##.#",
  "....#...##",
  "...##..##.",
  "...#.#####",
  ".#.####.#.",
  "..#..###.#",
  "..##.#..#.",
  "",
  "Tile 1489:",
  "##.#.#....",
  "..##...#..",
  ".##..##...",
  "..#...#...",
  "#####...#.",
  "#..#.#.#.#",
  "...#.#.#..",
  "##.#...##.",
  "..##.##.##",
  "###.##.#..",
  "",
  "Tile 2473:",
  "#....####.",
  "#..#.##...",
  "#.##..#...",
  "######.#.#",
  ".#...#.#.#",
  ".#########",
  ".###.#..#.",
  "########.#",
  "##...##.#.",
  "..###.#.#.",
  "",
  "Tile 2971:",
  "..#.#....#",
  "#...###...",
  "#.#.###...",
  "##.##..#..",
  ".#####..##",
  ".#..####.#",
  "#..#.#..#.",
  "..####.###",
  "..#.#.###.",
  "...#.#.#.#",
  "",
  "Tile 2729:",
  "...#.#.#.#",
  "####.#....",
  "..#.#.....",
  "....#..#.#",
  ".##..##.#.",
  ".#.####...",
  "####.#.#..",
  "##.####...",
  "##..#.##..",
  "#.##...##.",
  "",
  "Tile 3079:",
  "#.#.#####.",
  ".#..######",
  "..#.......",
  "######....",
  "####.#..#.",
  ".#...#.##.",
  "#.#####.##",
  "..#.###...",
  "..#.......",
  "..#.###..."]

type Tile = [String]
type Tiles = MAP.Map Int Tile
type Edge = String
type Edges = [Edge]
type EdgeMap = MAP.Map Int Edges

data Dir = DUp | DLeft | DDown | DRight deriving (Show, Eq, Enum, Bounded)

inputFileP :: Parser Tiles
inputFileP = MAP.fromList <$> (tileP `sepBy` newline)

tileP :: Parser (Int,Tile)
tileP = do
  string "Tile "
  id <- read <$> many1 digit
  string ":\n"
  ss <- count 10 (count 10 (oneOf "#.") <* newline)
  return (id, ss)

getEdge :: Dir -> Tile -> Edge
getEdge DUp = head
getEdge DDown = last
getEdge DRight = map last
getEdge DLeft = map head

getEdges :: Tile -> Edges
getEdges = sequence (getEdge <$> [DUp ..])

isUnmatchedEdge :: EdgeMap -> Int -> Edge -> Bool
isUnmatchedEdge em tileID e =
  not $ or $ MAP.mapWithKey otherContainsE em
  where
  inve = reverse e
  containsE es = e `elem` es || inve `elem` es
  otherContainsE j es = tileID /= j && containsE es

getTilesEdges = MAP.map getEdges

findCornerTileIDs :: Tiles -> [Int]
findCornerTileIDs allTiles = cornerTileIDs where
  cornerTileIDs = MAP.keys cornerTiles
  cornerTiles = MAP.filterWithKey hasTwoUnique allEdges
  hasTwoUnique :: Int -> Edges -> Bool
  hasTwoUnique i es =
    2 == length (filter (onlyIn i) es)
  onlyIn = isUnmatchedEdge allEdges
  allEdges = getTilesEdges allTiles


part1 :: String -> Either ParseError Int
part1 inp = do
  tiles <- parse inputFileP "" inp
  return $ product $ findCornerTileIDs tiles

testPart1 :: IO ()
testPart1 =
  assert (part1 testInput == Right 20899048083289) putStrLn "part1 worked"

rotateL :: Tile -> Tile
rotateL = reverse . transpose

rotateR :: Tile -> Tile
rotateR = transpose . reverse

applyN :: (a -> a) -> Int -> (a -> a)
applyN _ 0 = id
applyN f n = applyN f (n-1) . f

rotateLN n = applyN rotateL $ n `mod` 4
rotateRN n = applyN rotateR $ n `mod` 4

extractSingle [x] = x
extractSingle other = error $ "expected single entry, received " ++ show other

findCornerOrientation :: Tiles -> Int -> Tile
findCornerOrientation allTiles tileID =
  let tile = allTiles MAP.! tileID in
  let allOrients = [rotateLN n tile | n <- [0..3]] in
  --bottom & right need to be unique
  extractSingle $ filter botAndRightUniq allOrients
  where
    botAndRightUniq :: Tile -> Bool
    botAndRightUniq t = all uniq [bot, right]
      where bot = getEdge DDown t
            right = getEdge DRight t
            uniq = isUnmatchedEdge edges tileID
    edges = getTilesEdges allTiles

isqrt = floor . sqrt . fromIntegral -- square root with integers

allOrientations :: Tile -> [Tile]
allOrientations tile = [flipfun $ rotateLN n tile
                        | n <- [0..3], flipfun <- [id, reverse]]

findMatch :: Tiles -> Dir -> Edge -> (Int, Tile)
findMatch tiles dir edge = (tID, oriented)
  where
    allArrangements = MAP.map allOrientations tiles
    matched = MAP.map matches allArrangements
    (tID, (True,oriented)) = extractSingle $ MAP.toList $ MAP.filter fst matched
    matches orients =
      let idx = edge `elemIndex` (getEdge dir <$> orients) in
      case idx of
        Nothing -> (False, [])
        (Just idx') -> (True, orients !! idx')

opposite :: Dir -> Dir
opposite d = toEnum ((fromEnum d + 2) `mod` 4)

nextTile :: Dir -> (Tiles, Tile) -> (Tiles, Tile)
nextTile dir (rest, t) =
  let toMatch = getEdge dir t in
  let (nextID,oriented) = findMatch rest (opposite dir) toMatch in
  (MAP.delete nextID rest, oriented)

nextRow :: (Tiles, [Tile]) -> (Tiles, [Tile])
nextRow (tiles, under) =
  foldr extendUp (tiles, []) under
  where
    extendUp under (tiles, ord) =
      let (tiles', new) = nextTile DUp (tiles, under) in
      (tiles', new:ord)

buildImage :: Tiles -> [[Tile]]
buildImage tiles =
  let stTileID = head $ findCornerTileIDs tiles in
  let stTile = findCornerOrientation tiles stTileID in
  let imgdim = isqrt (MAP.size tiles) - 1 in
  let tiles' = MAP.delete stTileID tiles in
  let (tiles'',lastRow) = foldl prepTile (tiles',[stTile]) [1..imgdim] in
  snd $ foldl prepRow (tiles'', [lastRow]) [1..imgdim]
  where
    prepTile (ts,ord) _ =
      let (ts',new) = nextTile DLeft (ts, head ord) in
      (ts', new:ord)
    prepRow (ts,ord) _ =
      let (ts',new) = nextRow (ts, head ord) in
      (ts', new:ord)

showTiles :: [[Tile]] -> String -- [[[String]]]
showTiles tiles =
  let ts = map unwords . transpose <$> tiles in
  unlines $ unlines <$> ts


cropEdges :: Tile -> Tile
cropEdges = map (tail . init) . tail . init

mergeTiles :: [[Tile]] -> Tile
mergeTiles tiles =
  let cropped = map (map cropEdges) tiles in
  let merged = map concat . transpose <$> cropped in
  concat merged

monster = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

countMonsters :: Tile -> Int
countMonsters tile =
  let toCheck = [(x,y) | x <- [0..xmax - dx] , y <- [0..ymax - dy]] in
  length $ filter hasMonster toCheck
  where
    (xmax, ymax) = (length tile, length $ head tile)
    (dx, dy) = (length monster, length $ head monster)
    hasMonster (x,y) =
      let ss = itemIndices $ getSubSpace (x,y) in
      let monsterss = itemIndices monster in
      null $ concat $ zipWith (\\) monsterss ss
    itemIndices space =
      map (elemIndices '#') space
    getSubSpace (x,y) =
      let subx = take dx $ drop x tile in
      take dy . drop y <$> subx

getRoughness :: Tile -> Int
getRoughness tile =
  let nmonsters = countMonsters tile in
  count' tile - count' monster * nmonsters
  where count' = length . concatMap (elemIndices '#')

part2 :: String -> Either ParseError Int
part2 inp = do
  tiles <- parse inputFileP "" inp
  let image = buildImage tiles
  return $ getRoughness $ mergeTiles image

testPart2 :: IO ()
testPart2 = assert (part2 testInput == Right 273) putStrLn "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input

