import Text.ParserCombinators.Parsec
import Control.Exception (assert)
import Data.List (intersect, nub)

type PersonAnswers = String
type GroupAnswers = [PersonAnswers]

testInput = unlines
  ["abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b"]

inputFileP :: GenParser Char st [GroupAnswers]
inputFileP = groupAnswersP `sepBy` emptyLineP

emptyLineP = string "\n\n"

groupAnswersP = personAnswersP `sepEndBy` answerDelimP

answerDelimP = do
  notFollowedBy emptyLineP
  char '\n'

personAnswersP = many1 lower

testParse :: IO ()
testParse = do
  assert (parse inputFileP "wurst" testInput ==
          Right [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]])
    return ()

nUniqueAnswers :: GroupAnswers -> Int
nUniqueAnswers = length . nub . concat

part1 :: String -> Either ParseError Int
part1 inp = do
  grpans <- parse inputFileP "error" inp
  return $ sum $ map nUniqueAnswers grpans

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Right 11) return ()

--for part2
nUbiquitousAnswers :: GroupAnswers -> Int
nUbiquitousAnswers = length . foldl1 intersect

part2 :: String -> Either ParseError Int
part2 inp = do
  grpans <- parse inputFileP "error" inp
  return $ sum $ map nUbiquitousAnswers grpans

testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Right 6) return ()

main = do
  testParse
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
