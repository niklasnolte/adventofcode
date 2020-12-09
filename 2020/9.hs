import qualified Data.Dequeue as DEQ
import qualified Data.Vector as V
import qualified Data.List as L
import Debug.Trace(trace)
import Control.Monad(guard)
import Control.Exception(assert)
import qualified Control.Monad.State as ST

testInput = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"

tr x = trace (show x) x

type Stream = V.Vector Int
type IntSeq = DEQ.BankersDequeue Int

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
                          ST.State (IntSeq, Int, Int) ()
findConsecWithRightSum sum stream = do
  (deq, frontIdx, deqSum) <- ST.get
  let endIdx = frontIdx + length deq - 1
  case compare deqSum sum of
    EQ -> return ()
    LT -> do
      let newElem = stream V.! (endIdx + 1)
      ST.put (DEQ.pushBack deq newElem,
              frontIdx,
              deqSum+newElem)
      findConsecWithRightSum sum stream
    GT -> do
      let Just (v,deq') = DEQ.popFront deq
      ST.put (deq', frontIdx + 1, deqSum - v)
      findConsecWithRightSum sum stream

part2 :: Int -> String -> Maybe Int
part2 n inp = do
  let stream = readInput inp
  idx <- findMismatchedIdx n stream
  let find = findConsecWithRightSum (stream V.! idx) stream
  let (deq, _, _) = ST.execState find (DEQ.empty, 0, 0)
  let low = minimum deq
  let high = maximum deq
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
