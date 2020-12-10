import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad(guard)
import Control.Exception(assert)
import qualified Control.Monad.State as ST

testInput = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"

type Stream = V.Vector Int

readInput :: String -> Stream
readInput = V.fromList . map read . lines

isSumOfTwoInLastN :: Int -> Int -> Stream -> Bool
isSumOfTwoInLastN n idx s =
  let streamSlice = V.slice (idx-n) n s in
  let allSums = do
        x <- streamSlice
        y <- streamSlice
        guard $ x /= y
        return $ x+y
  in
  (s V.! idx) `notElem`  allSums

findMismatchedIdx :: Int -> Stream -> Maybe Int
findMismatchedIdx n stream = do
  L.find (\i -> isSumOfTwoInLastN n i stream)
         [n..(V.length stream - 1)]

part1 :: Int -> String -> Maybe Int
part1 n inp = do
  let stream = readInput inp
  (stream V.!) <$> findMismatchedIdx n stream

testPart1 :: IO()
testPart1 = assert (part1 5 testInput == Just 127) (putStrLn "part1 works")

findConsecWithRightSum :: Int ->
                          Stream ->
                          ST.State (Int, Int, Int) ()
findConsecWithRightSum targetSum fullstream = do
  (begin, end, sum) <- ST.get
  -- begin = start of slice
  -- end = last of slice + 1
  case compare sum targetSum of
    EQ -> return ()
    LT -> do
      ST.put (begin, end+1, sum + fullstream V.! end)
      findConsecWithRightSum targetSum fullstream
    GT -> do
      ST.put (begin+1, end, sum - fullstream V.! begin)
      findConsecWithRightSum targetSum fullstream

part2 :: Int -> String -> Maybe Int
part2 n inp = do
  let stream = readInput inp
  idx <- findMismatchedIdx n stream
  let find = findConsecWithRightSum (stream V.! idx) stream
  let (begin, end, _) = ST.execState find (0, 0, 0)
  let slice = V.slice begin (end-begin-1) stream
  let low = minimum slice
  let high = maximum slice
  return $ low + high

testPart2 :: IO()
testPart2 =
  assert (part2 5 testInput == Just 62) (putStrLn "part2 works")

main :: IO()
main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 25 input
  print $ part2 25 input
