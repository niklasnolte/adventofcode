import Text.ParserCombinators.Parsec (char, endBy, many, digit, space, lower, GenParser, string, parse)
import Control.Exception (assert)

data Policy = Policy {
                getMin :: Int,
                getMax :: Int,
                getChar :: Char
              } deriving (Show, Eq)

inputFileP :: GenParser Char st [(Policy, String)]
inputFileP = lineP `endBy` char '\n'

lineP :: GenParser Char st (Policy, String)
lineP = do
  policy <- policyP
  string ": "
  s <- many lower
  return (policy, s)

policyP :: GenParser Char st Policy
policyP = do
  min <- read <$> many digit
  char '-'
  max <- read <$> many digit
  space
  Policy min max <$> lower

-- omg, parsec is so great

testParse :: IO ()
testParse = do
  let testInput = unlines ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
  let result = [(Policy 1 3 'a', "abcde"), (Policy 1 3 'b', "cdefg"), (Policy 2 9 'c', "ccccccccc")]
  let x = parse inputFileP "unknown" testInput
  assert (x == Right result) $ return ()


isValidUnderPolicy1 :: String -> Policy -> Bool
isValidUnderPolicy1 s (Policy pMin pMax pChar) =
  length (filter (==pChar) s) `elem` [pMin..pMax]


testIsValidUnderPolicy1 :: IO ()
testIsValidUnderPolicy1 = do
  let p = Policy 1 3 'a'
  let s1 = "aabbcc"
  let s2 = "cdefag"
  let s3 = "caaxxaag"
  assert (isValidUnderPolicy1 s1 p) $
    assert (isValidUnderPolicy1 s2 p) $
    assert (not $ isValidUnderPolicy1 s3 p) $
    return ()


part1or2 :: (String -> Policy -> Bool) -> String -> Maybe Int
part1or2 isValid input = do
  let parsed = parse inputFileP "unknown" input
  case parsed of
    Right x -> Just $ length $ filter (\(p,s) -> isValid s p) x
    Left _ -> Nothing


xor :: Bool -> Bool -> Bool
xor b1 b2 =
  (b1 || b2) && not (b1 && b2)


isValidUnderPolicy2 :: String -> Policy -> Bool
isValidUnderPolicy2  s (Policy pMin pMax pChar) =
  let p1 = pMin - 1 in
  let p2 = pMax - 1 in
  (s !! p1 == pChar) `xor` (s !! p2 == pChar)


testIsValidUnderPolicy2 :: IO ()
testIsValidUnderPolicy2 = do
  let p = Policy 1 3 'a'
  let s1 = "aabbcc"
  let s2 = "cdefag"
  let s3 = "aaaxxaag"
  assert (isValidUnderPolicy2 s1 p) $
    assert (not $ isValidUnderPolicy2 s2 p) $
    assert (not $ isValidUnderPolicy2 s3 p) $
    return ()

part1 = part1or2 isValidUnderPolicy1
part2 = part1or2 isValidUnderPolicy2

main = do
  testParse
  testIsValidUnderPolicy1
  testIsValidUnderPolicy2
  input <- getContents
  print $ part1 input
  print $ part2 input
