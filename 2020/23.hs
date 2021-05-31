import Control.Exception (assert)
import Data.IntMap ((!))
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L

-- the key is the current cup, and it points to the next one in the sequence
type Cups = IM.IntMap Int

modDec :: Int -> Int -> Int
modDec _ 0 = error "operation only supported for values [1..nMax]"
modDec nMax 1 = nMax
modDec _ i = i -1

singleMove :: Int -> (Int, Cups) -> (Int, Cups)
-- make a single move in the game
singleMove nMax (current, cups) = (newAfterCurrent, newCups'')
  where
    next1 = cups ! current
    next2 = cups ! next1
    next3 = cups ! next2
    newAfterCurrent = cups ! next3
    dest = findDest cups current
    newCups = IM.insert next3 (cups ! dest) cups
    newCups' = IM.insert dest next1 newCups
    newCups'' = IM.insert current newAfterCurrent newCups'

    findDest :: Cups -> Int -> Int
    findDest cups c =
      if d == next1 || d == next2 || d == next3
        then findDest cups d
        else d
      where
        d = modDec nMax c

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

cupsFromList :: [IM.Key] -> IM.IntMap IM.Key
-- IntMap, where each key points to the next element in the cup sequence
cupsFromList cups = IM.fromList $ zip cups $ tail $ cycle cups

digitsFromString :: String -> [Int]
-- read ints from a string of single digit ints (0-9)
digitsFromString = L.map (\x -> read [x] :: Int)

part1 :: String -> Int
part1 inp =
  extractAnswer $ snd $ applyN 100 (singleMove $ length cupOrder) (head cupOrder, cups)
  where
    cupOrder = digitsFromString inp
    cups = cupsFromList cupOrder
    extractAnswer :: Cups -> Int
    extractAnswer cups = read $ concatMap show sorted
      where
        sorted = tail $ take (IM.size cups) $ iterate (cups !) 1

testInput :: String
testInput = "389125467"

testPart1 :: IO ()
testPart1 = assert (part1 testInput == 67384529) putStrLn "part1 worked"

part2 :: String -> Int
part2 inp = extractAnswer finalCups
  where
    nMax = 1000000
    finalCups = snd $ applyN 10000000 (singleMove nMax) (head firstCups, cups)
    firstCups = digitsFromString inp
    allCups = firstCups ++ [(maximum firstCups + 1) .. nMax]
    cups = cupsFromList allCups
    extractAnswer :: Cups -> Int
    extractAnswer cups = product $ take 2 $ tail $ iterate (cups !) 1

testPart2 :: IO ()
testPart2 = assert (part2 testInput == 149245887792) putStrLn "part2 worked"

main :: IO ()
main = do
  let input = "186524973"
  testPart1
  print $ part1 input
  testPart2
  print $ part2 input
