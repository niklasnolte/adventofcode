import Debug.Trace (trace)
import Control.Exception (assert)
import Data.List(sort, find)

tr :: (Show a) => a -> a
tr x = trace (show x) x

data RowSelector = TakeFront | TakeBack deriving (Enum, Show)
data ColSelector = TakeLeft | TakeRight deriving (Enum, Show)
type Partition = ([RowSelector], [ColSelector])

toRowSelector :: Char -> Maybe RowSelector
toRowSelector 'B' = Just TakeBack
toRowSelector 'F' = Just TakeFront
toRowSelector _ = Nothing

toColSelector :: Char -> Maybe ColSelector
toColSelector 'L' = Just TakeLeft
toColSelector 'R' = Just TakeRight
toColSelector _ = Nothing

toPartition :: String -> Maybe Partition
toPartition s = do
  let rows = take 7 s
  let cols = drop 7 s
  rs <- mapM toRowSelector rows
  cs <- mapM toColSelector cols
  return (rs, cs)

testInput = unlines ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

testToPartition :: IO ()
testToPartition = do
  print $ map toPartition (lines testInput)

type Seat = (Int, Int, Int)
type Range = (Int, Int)

binaryPartition :: [Int] -> Range -> Maybe Int
binaryPartition [] (l,h) = 
  if l == h
  then Just l
  else Nothing
binaryPartition (r:rs) (l,h) = case r of
  0 -> binaryPartition rs (l, half)
  1 -> binaryPartition rs (half+1, h)
  where half = (h+l) `div` 2

binaryPartitionRows :: [RowSelector] -> Maybe Int
binaryPartitionRows rs =
  binaryPartition (map fromEnum rs) (0, 127)

binaryPartitionCols :: [ColSelector] -> Maybe Int
binaryPartitionCols cs =
  binaryPartition (map fromEnum cs) (0, 7)

toSeat :: Partition -> Maybe Seat
toSeat (rows, cols) = do
  row <- binaryPartitionRows rows
  col <- binaryPartitionCols cols
  return (row, col, row*8+col)

getSeatIdsFromInput :: String -> Maybe [Int]
getSeatIdsFromInput input =
  let sids = do
        i <- lines input
        return $ do
            p <- toPartition i
            (_,_,id) <- toSeat p
            return id
  in sequence sids

part1 :: String -> Maybe Int
part1 inp = maximum <$> getSeatIdsFromInput inp

testPart1 :: IO ()
testPart1 =
  assert (Just 820 == part1 testInput) return ()

part2 :: String -> Maybe Int
part2 inp = do
  ids <- sort <$> getSeatIdsFromInput inp
  let min = head ids
  let max = last ids
  let range = [min..max]
  (_,ret) <- find (uncurry (/=)) (zip ids range)
  return ret

main = do
  input <- getContents
  testToPartition
  testPart1
  print $ part1 input
  print $ part2 input
