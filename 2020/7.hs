import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Data.Maybe(fromMaybe)
import Control.Exception(assert)

testInput = unlines [
  "light red bags contain 1 bright white bag, 2 muted yellow bags.",
  "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
  "bright white bags contain 1 shiny gold bag.",
  "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
  "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
  "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
  "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
  "faded blue bags contain no other bags.",
  "dotted black bags contain no other bags."]

testInput2 = unlines [
  "shiny gold bags contain 2 dark red bags.",
  "dark red bags contain 2 dark orange bags.",
  "dark orange bags contain 2 dark yellow bags.",
  "dark yellow bags contain 2 dark green bags.",
  "dark green bags contain 2 dark blue bags.",
  "dark blue bags contain 2 dark violet bags.",
  "dark violet bags contain no other bags."]


type Color = String
type BagContent = Set.Set (Color, Int)
type Rule = (Color, BagContent)
type Rules = Map.Map Color BagContent
type InvRules = Map.Map Color (Set.Set Color)

inputFileP :: GenParser Char st Rules
inputFileP = Map.fromList <$> (ruleP `endBy` newline)

ruleP :: GenParser Char st Rule
ruleP = do
  c <- colorP
  string " bags contain"
  content <- try noBagContentP <|> try bagContentP
  return (c, content)

colorP :: GenParser Char st Color
colorP = do
  first <- many1 lower
  delim <- string " "
  second <- many1 lower
  return $ first ++ delim ++ second

noBagContentP :: GenParser Char st BagContent
noBagContentP = string " no other bags." >> return mempty

bagContentP :: GenParser Char st BagContent
bagContentP = do
  char ' '
  num <- many1 digit
  char ' '
  color <- colorP
  char ' '
  many1 letter
  delim <- oneOf ",."
  case delim of
    ',' -> Set.insert (color, read num) <$> bagContentP
    '.' -> return $ Set.singleton (color, read num)


invertRules :: Rules -> InvRules
invertRules rules =
  let rs = Map.toList rules in
  foldl insert mempty rs
  where
    insert :: InvRules -> Rule -> InvRules
    insert invMap (bagColor,containedColors) =
      foldl (insertOneRule bagColor) invMap containedColors
    insertOneRule bc = \map (color,_) ->
      Map.insertWith Set.union color (Set.singleton bc) map

whoContainsMe :: Color -> InvRules -> Set.Set Color
whoContainsMe color cmap =
  let directMothers = fromMaybe mempty $ Map.lookup color cmap in
  let nextMothers = Set.map (`whoContainsMe` cmap) directMothers in
  Set.unions $ Set.insert directMothers nextMothers

nContainedBags :: Color -> Rules -> Int
nContainedBags color rules =
  let (Just contents) = Map.lookup color rules in
  foldl addNextColor 0 contents
  where addNextColor :: Int -> (Color, Int) -> Int
        addNextColor bagSum (c,n) =
          bagSum + n + n*nContainedBags c rules

part1 :: Rules -> Int
part1 = length . whoContainsMe "shiny gold" . invertRules

testPart1 :: IO ()
testPart1 = do
  let (Right x) = parse inputFileP "" testInput
  assert (part1 x == 4) return ()

part2 :: Rules -> Int
part2 = nContainedBags "shiny gold"

testPart2 :: IO ()
testPart2 = do
  let (Right x) = parse inputFileP "" testInput
  let (Right y) = parse inputFileP "" testInput2
  assert (part2 x == 32) $
    assert (part2 y == 126) return ()

main = do
  testPart1
  testPart2
  input <- getContents
  let parsd = parse inputFileP "crap" input
  case parsd of
    (Left x) -> print x
    (Right x) -> do
      print $ part1 x
      print $ part2 x
