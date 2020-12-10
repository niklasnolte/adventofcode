import Data.List(sort)
import Data.List.Split(splitOn)
import Control.Exception(assert)

testInput = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

parseInput :: String -> [Int]
parseInput = map read . lines

getDifferences :: [Int] -> [Int]
getDifferences l =
  let srtd = sort (0:l) in
  [x - y | (x,y) <- zip (tail srtd) (init srtd)]

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

part1 :: String -> Int
part1 inp =
  let diffs = getDifferences $ parseInput inp in
  count 1 diffs * (count 3 diffs + 1)

testPart1 :: IO ()
testPart1 = assert (part1 testInput == 220) $ print "part1 worked"

countArrangements :: [Int] -> Int
countArrangements diffs =
  let toCountOn = splitOn [3] diffs in
  product $ map count toCountOn
  where
    count :: [Int] -> Int
    count [] = 1
    count [_] = 1
    count (i:j:diffs)
      | i + j <= 3 = count (i+j:diffs) + count (j:diffs)
      | i + j > 3 = count (j:diffs)

part2 :: String -> Int
part2 inp =
  let diffs = getDifferences $ parseInput inp in
  countArrangements diffs

testPart2 :: IO ()
testPart2 = assert (part2 testInput == 19208) $ print "part2 worked"

main = do
  testPart1
  testPart2
  inp <- getContents
  print $ part1 inp
  print $ part2 inp
