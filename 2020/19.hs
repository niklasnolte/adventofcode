import qualified Data.Map as MAP
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Monad(void, guard)
import Control.Exception (assert)
import Debug.Trace (trace)

tr x = trace (show x) x

data Rule = And Rule Rule | Or Rule Rule | Letter Char | Refer Int deriving (Show, Eq)
type Rules = MAP.Map Int Rule

inputFileP :: Parser (Rules, [String])
inputFileP = (,) <$> rulesP <*> (char '\n' >> restP)

rulesP :: Parser Rules
rulesP = MAP.fromList <$> ruleP `endBy` newline

restP :: Parser [String]
restP = many1 letter `sepEndBy` newline

intP = read <$> many1 digit

ruleP :: Parser (Int, Rule)
ruleP = do
  rulei <- intP
  string ": "
  rule <- buildExpressionParser table term
  return (rulei, rule)
  where
    table = [[Infix (optional (char ' ') >> return And) AssocLeft],
             [Infix (string "| " >> return Or) AssocLeft]
            ]
    term = ((Refer <$> intP) <|> (Letter <$> letterP)) <* optional (char ' ')
    letterP = char '"' *> anyChar <* char '"'

buildRuleParser :: Rules -> Rule -> Parser ()
buildRuleParser rules r = case r of
  (Letter c) -> void $ char c
  (Refer i) -> build (rules MAP.! i)
  (And lhs rhs) -> build lhs >> build rhs
  (Or lhs rhs) -> try (build lhs) <|> build rhs
  where
    build = buildRuleParser rules
  

part1 :: String -> Either ParseError Int
part1 input = do
  (rules, inps) <- parse inputFileP "" input
  let r0 = buildRuleParser rules (Refer 0) <* eof
  return $ sum $ do
    inp <- inps
    case parse r0 "" inp of
      (Right _) -> return 1
      (Left _) -> return 0

testInput = unlines
  ["0: 4 1 5",
   "1: 2 3 | 3 2",
   "2: 4 4 | 5 5",
   "3: 4 5 | 5 4",
   "4: \"a\"",
   "5: \"b\"",
   "",
   "ababbb",
   "bababa",
   "abbbab",
   "aaabbb",
   "aaaabbb"]

testInput2 = unlines
  ["42: 9 14 | 10 1",
   "9: 14 27 | 1 26",
   "10: 23 14 | 28 1",
   "1: \"a\"",
   "11: 42 31",
   "5: 1 14 | 15 1",
   "19: 14 1 | 14 14",
   "12: 24 14 | 19 1",
   "16: 15 1 | 14 14",
   "31: 14 17 | 1 13",
   "6: 14 14 | 1 14",
   "2: 1 24 | 14 4",
   "0: 8 11",
   "13: 14 3 | 1 12",
   "15: 1 | 14",
   "17: 14 2 | 1 7",
   "23: 25 1 | 22 14",
   "28: 16 1",
   "4: 1 1",
   "20: 14 14 | 1 15",
   "3: 5 14 | 16 1",
   "27: 1 6 | 14 18",
   "14: \"b\"",
   "21: 14 1 | 1 14",
   "25: 1 1 | 1 14",
   "22: 14 14",
   "8: 42",
   "26: 14 22 | 1 20",
   "18: 15 15",
   "7: 14 5 | 1 21",
   "24: 14 1",
   "",
   "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
   "bbabbbbaabaabba",
   "babbbbaabbbbbabbbbbbaabaaabaaa",
   "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
   "bbbbbbbaaaabbbbaaabbabaaa",
   "bbbababbbbaaaaaaaabbababaaababaabab",
   "ababaaaaaabaaab",
   "ababaaaaabbbaba",
   "baabbaaaabbaaaababbaababb",
   "abbbbabbbbaaaababbbbbbaaaababb",
   "aaaaabbaabaaaaababaa",
   "aaaabbaaaabbaaa",
   "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
   "babaaabbbaaabaababbaabababaaab",
   "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]

testPart1 = do
  assert (part1 testInput == Right 2) $ putStrLn "part1 worked"
  assert (part1 testInput2 == Right 3) $ putStrLn "part1 worked"

part2 :: String -> Either ParseError Int
part2 input = do
  (rules, inps) <- parse inputFileP "" input
  let r42 = buildRuleParser rules (Refer 42)
  let r31 = buildRuleParser rules (Refer 31)
  let r0 = do
      n42s <- many1 (try r42)
      n31s <- many1 r31
      eof
      guard (length n42s > length n31s)
  return $ sum $ do
    inp <- inps
    case parse r0 "" inp of
      (Right _) -> return 1
      (Left _) -> return 0

testPart2 = do
  assert (part2 testInput2 == Right 12) $ putStrLn "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
