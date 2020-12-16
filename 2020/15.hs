import Data.List.Split(splitOn)
import Control.Exception(assert)
import Text.Read(readEither)
import qualified Data.Map as MAP

data GameState = GameState { getRound :: Int,
                             getThisNum :: Int,
                             getHist :: MAP.Map Int Int } deriving (Show)

speakNum :: GameState -> GameState
speakNum (GameState cRound thisNum hist) =
  GameState (cRound+1) newNum (MAP.insert thisNum cRound hist)
  where newNum = case MAP.lookup thisNum hist of
          Nothing -> 0
          Just lastTime -> cRound - lastTime

getInitHist :: [Int] -> GameState
getInitHist is =
  GameState { getRound = length is - 1,
              getThisNum = last is,
              getHist = MAP.fromList $ zip (init is) [0..] }


speakNTimes :: Int -> GameState -> GameState
speakNTimes 0 = id
speakNTimes i = speakNTimes (i-1) . speakNum

parseInput :: String -> Either String [Int]
parseInput = mapM readEither . splitOn ","

part1 :: String -> Either String Int
part1 inp = do
  input <- parseInput inp
  let initHist = getInitHist input
  let roundsToPlay = 2020 - getRound initHist - 1
  return $ getThisNum $ speakNTimes roundsToPlay initHist

testInput = "0,3,6"

testPart1 :: IO ()
testPart1 =
  assert (part1 testInput == Right 436) $ putStrLn "part1 worked"

part2 :: String -> Either String Int
part2 inp = do
  input <- parseInput inp
  let initHist = getInitHist input
  let roundsToPlay = 30000000 - getRound initHist - 1
  return $ getThisNum $ speakNTimes roundsToPlay initHist

testPart2 :: IO ()
testPart2 =
  assert (part2 testInput == Right 175594) $ putStrLn "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
