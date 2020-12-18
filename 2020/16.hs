import Text.ParserCombinators.Parsec
import Control.Exception(assert)
import Data.List (isPrefixOf, transpose, groupBy, sortBy,nub)
import Control.Monad(guard)

headEither (x:_) = Right x
headEither [] = Left "head from empty list"


type Ticket = [Int]
type Constraint = [(Int, Int)]
type Rule = (String, Constraint)

inputFileP :: Parser ([Rule], Ticket, [Ticket])
inputFileP = do
  rules <- ruleP `endBy` char '\n'
  string "\nyour ticket:\n"
  myticket <- ticketP
  string "\n\nnearby tickets:\n"
  nearbys <- ticketP `endBy` char '\n'
  eof
  return (rules, myticket, nearbys)

ticketP :: Parser Ticket
ticketP = map read <$> many digit `sepBy` char ','

ruleP :: Parser Rule
ruleP = do
  head <- many (letter <|> char ' ')
  string ": "
  ranges <- rangeP `sepBy` string " or "
  return (head, ranges)

rangeP :: Parser (Int, Int)
rangeP = do
  from <- read <$> many digit
  char '-'
  to <- read <$> many digit
  return (from, to)

testInput = unlines
  ["class: 1-3 or 5-7",
   "row: 6-11 or 33-44",
   "seat: 13-40 or 45-50",
   "",
   "your ticket:",
   "7,1,14",
   "",
   "nearby tickets:",
   "7,3,47",
   "40,4,50",
   "55,2,20",
   "38,6,12"]

isBetween :: Int -> (Int, Int) -> Bool
isBetween i (l,h) = i <= h && i >= l

matchesConstraint :: Constraint -> Int -> Bool
matchesConstraint c n =
  any (isBetween n) c

invalidNumsInTicket :: [Constraint] -> Ticket -> [Int]
invalidNumsInTicket constrs = filter (not . isValidNum)
  where
    isValidNum :: Int -> Bool
    isValidNum i = any (`matchesConstraint` i) constrs

part1 :: String -> Either ParseError Int
part1 inp = do
  (rules, _, nearbyTickets) <- parse inputFileP "" inp
  let constraints = map snd rules
  return $ sum $ concatMap (invalidNumsInTicket constraints) nearbyTickets

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Right 71) $ putStrLn "part1 worked"

testInput2 = unlines
  ["departure class: 0-1 or 4-19",
   "row: 0-5 or 8-19",
   "departure seat: 0-13 or 16-19",
   "",
   "your ticket:",
   "11,12,13",
   "",
   "nearby tickets:",
   "3,9,18",
   "15,1,5",
   "5,14,9" ]

isValidTicket :: [Constraint] -> Ticket -> Bool
isValidTicket c t = null $ invalidNumsInTicket c t

getUniqCombs :: [[(String, Int)]] -> [[(String, Int)]]
getUniqCombs [] = [[]]
getUniqCombs (x:rest) = do
  xi <- x
  r <- getUniqCombs rest
  guard $ snd xi `notElem` map snd r
  return (xi:r)

resolveOrderFromMatches :: [(String, Int)] -> Either String [String]
resolveOrderFromMatches [] = Left "cannot resolve empty list"
resolveOrderFromMatches matches =
  let srtdMatches = sortBy (onFirsts compare) matches in
  let gMatches = groupBy (onFirsts (==)) srtdMatches in
  let allOrders = getUniqCombs gMatches in
  let valids = filter isValidOrder allOrders in
  map fst . sortBy (onSeconds compare)  <$> headEither valids
  where
    onFirsts f (i,_) (j,_) = f i j
    onSeconds f (_,i) (_,j) = f i j
    isValidOrder :: [(String, Int)] -> Bool
    isValidOrder order =
      length (nub $ snd <$> order) == length order


associatePosToRules :: [Rule] -> [Ticket] -> Either String [String]
associatePosToRules rules tickets =
  let numss = transpose tickets in
  let matching = do
        (name,constr) <- rules
        (pos,i) <- zip numss [0..]
        guard $ all (matchesConstraint constr) pos
        return (name, i)
  in
  resolveOrderFromMatches matching

part2 :: String -> Either String Int
part2 inp = do
  let parsed = parse inputFileP "" inp
  case parsed of
    (Left x) -> Left $ show x
    (Right (rules, myticket, nearbyTickets)) -> do
      let validtickets = filter (isValidTicket $ snd <$> rules) nearbyTickets
      order <- associatePosToRules rules validtickets
      return $ product [ n | (n,r) <- zip myticket order,
                                      "departure" `isPrefixOf` r]

testPart2 :: IO ()
testPart2 = 
  assert (part2 testInput2 == Right 156) $ putStrLn "part2 worked"


main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
