import Control.Exception (assert)
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.ParserCombinators.Parsec

tr :: Show a => a -> a
tr x = trace (show x) x

type Deck = [Int]

testInput :: String
testInput =
  unlines
    [ "Player 1:",
      "9",
      "2",
      "6",
      "3",
      "1",
      "",
      "Player 2:",
      "5",
      "8",
      "4",
      "7",
      "10"
    ]

inputFileP :: Parser (Deck, Deck)
inputFileP = do
  string "Player 1:\n"
  deck1 <- deckP
  string "\nPlayer 2:\n"
  deck2 <- deckP
  return (deck1, deck2)

deckP :: Parser Deck
deckP = do
  deck <- many1 digit `endBy` newline
  return $ map read deck

playGame :: (Deck, Deck) -> Deck
playGame (x, []) = x
playGame ([], x) = x
playGame (x : xs, y : ys) =
  playGame $
    if x > y -- bigger than?
      then (xs ++ [x, y], ys)
      else (xs, ys ++ [y, x])

getScore :: Deck -> Int
getScore d = sum $ zipWith (*) (reverse d) [1 ..]

part1 :: String -> Either ParseError Int
part1 inp = do
  decks <- parse inputFileP "" inp
  return $ getScore $ playGame decks

testPart1 :: IO ()
testPart1 = assert (part1 testInput == Right 306) putStrLn "part1 worked"

playRecursiveCombat :: S.Set (Deck, Deck) -> (Deck, Deck) -> (Int, Deck) -- (Winner, Deck)
playRecursiveCombat _ (x, []) = (1, x)
playRecursiveCombat _ ([], x) = (2, x)
playRecursiveCombat alreadyPlayed decks@(x : xs, y : ys) =
  if decks `S.member` alreadyPlayed
    then -- we have already played this configuration, infinite game rule kicks in
      (1, [])
    else -- continue playing the game, remember to put the current configuration
    -- into the memory to avoid playing the infinite game
      playRecursiveCombat (S.insert decks alreadyPlayed) nextdecks
  where
    winner
      | x <= length xs && y <= length ys =
        -- play the subgame, with a fresh memory
        fst $ playRecursiveCombat S.empty (take x xs, take y ys)
      -- or continue with the normal game
      | x > y = 1
      | y > x = 2
      | otherwise = error "cards have same value, there is no rule for that"
    nextdecks
      | winner == 1 = (xs ++ [x, y], ys)
      | winner == 2 = (xs, ys ++ [y, x])

part2 :: String -> Either ParseError Int
part2 inp = do
  decks <- parse inputFileP "" inp
  return $ getScore $ snd $ playRecursiveCombat S.empty decks

testPart2 :: IO ()
testPart2 = assert (part2 testInput == Right 291) putStrLn "part2 worked"

main :: IO ()
main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
