import Data.List(minimumBy, sortBy)
import Data.List.Split(splitOn)
import Debug.Trace(trace)
import Control.Exception(assert)
import Text.Read(readEither)

tr x = trace (show x) x

type Bus = Int
type TimeStamp = Int

testInput = "939\n7,13,x,x,59,x,31,19"

parseInput :: String -> Either String (TimeStamp, [(TimeStamp, Bus)])
parseInput str = do
  let [ts,rest] = lines str
  let readVals x = if x == "x" then Right 1 else readEither x
  buss <- mapM readVals $ splitOn "," rest
  return (read ts, filter (\(_,x) -> x /= 1) $ zip [0..] buss)

getTD :: TimeStamp -> Bus -> TimeStamp
getTD ts b = let r = (ts `mod` b) in -r + b*signum r

getBestBusWithDelay :: TimeStamp -> [Bus] -> (Bus,TimeStamp)
getBestBusWithDelay ts buss =
  let compareTDs = \(_,td1) (_,td2) -> compare td1 td2 in
  minimumBy compareTDs $ zip buss $ map (getTD ts) buss

part1 :: String -> Either String Int
part1 inp = do
  (ts, buss) <- parseInput inp
  let (bus,td) = getBestBusWithDelay ts $ map snd buss
  return $ bus*td

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Right 295) $ putStrLn "part1 worked"

findSmallestTS :: [(TimeStamp, Bus)] -> TimeStamp -> TimeStamp -> TimeStamp
findSmallestTS buss currentTS step =
  if all (\(i,b) -> getTD currentTS b == i `mod` b) buss
  then currentTS
  else findSmallestTS buss (currentTS+step) step

findTSForP2 :: [(TimeStamp, Bus)] -> TimeStamp
findTSForP2 buss =
  iterateSteps 1 ((-1) * fst (head srtdBuss))
  where
    srtdBuss = sortBy (\(_, b) (_,b2) -> compare b2 b) buss
    iterateSteps :: Int -> TimeStamp -> TimeStamp
    iterateSteps i currentTS =
      if i < length srtdBuss
      then
        let step = foldl1 lcm $ take i $ map snd srtdBuss in
        let nextTS = findSmallestTS (take (i+1) srtdBuss) currentTS step in
        iterateSteps (i+1) nextTS
      else currentTS


part2 :: String -> Either String Int
part2 inp = do
  (_, buss) <- parseInput inp
  return $ findTSForP2 buss

testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Right 1068781) $ putStrLn "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
