import qualified Data.Map as MAP
import Text.ParserCombinators.Parsec
import Text.Read (readEither)
import Control.Exception (assert)
import Debug.Trace (trace)

tr x = trace (show x) x

type Constraint = Parser String
type ConstraintRepr = [String]
type Rule = (Int, Constraint)
type RuleRepr = (Int, [ConstraintRepr])
type RulesRepr = MAP.Map Int [ConstraintRepr]

inputFileP :: Parser (RulesRepr, [String])
inputFileP = (,) <$> rulesP <*> (char '\n' >> restP)

restP :: Parser [String]
restP = many1 letter `sepEndBy` newline

rulesP :: Parser RulesRepr
rulesP = MAP.fromList <$> ruleP `endBy` newline

ruleP :: Parser RuleRepr
ruleP = do
  ident <- read <$> numP
  string ": "
  constrs <- constraintP `sepBy` string "| "
  return (ident, constrs)

numP :: Parser String
numP = many1 digit

constraintP :: Parser ConstraintRepr
constraintP = 
  try (numP <|> stringP) `sepEndBy` char ' '

stringP :: Parser String
stringP = do 
  char '"'
  s <- many1 letter
  char '"'
  return s

buildConstrFrom :: RulesRepr -> ConstraintRepr -> Maybe Constraint
buildConstrFrom rules repr =
  let readRules = mapM readEither repr :: Either String [Int] in
  case readRules of
    -- break if its a string rule
    (Left _) -> return (string $ head repr :: Parser String)
    -- its a ref to other rules
    (Right x) -> do
      nextRules <- mapM (`buildConstraint` rules) x
      return $ foldl1 combinetwo nextRules
  where
    combinetwo x y = do
      a <- x
      b <- y
      return $ a ++ b


buildConstraint :: Int -> RulesRepr -> Maybe Constraint
buildConstraint i rules = do
  repr <- MAP.lookup i rules
  constrs <- mapM (buildConstrFrom rules) repr
  return $ choice $ map try constrs

buildFullConstraint :: Int -> RulesRepr -> Maybe Constraint
buildFullConstraint i rules = do
  constr <- buildConstraint i rules
  return $ constr >>= (\x -> eof >> return x)

testInput = unlines
  [ "0: 4 1 5",
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
    "aaaabbb"
   ]


isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True



part1 :: String -> Either String Int
part1 str = do 
  let parsed = parse inputFileP "" str
  case parsed of
    (Left er) -> Left $ show er
    (Right (rules, rest)) ->
      case buildFullConstraint 0 rules of
        Nothing -> Left "could not build rule 0"
        (Just p) -> Right $ length $ filter isRight $ map (parse p "") rest

testPart1 :: IO ()
testPart1 =
  assert (part1 testInput == Right 2) $ putStrLn "part1 worked"

main = do
  testPart1
  input <- getContents
  print $ part1 input
