import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
    ( char,
      letter,
      newline,
      space,
      string,
      endBy,
      sepBy,
      many,
      parse,
      ParseError,
      Parser )

type Food = (S.Set Ingredient, S.Set Allergen)

type Ingredient = String

type Allergen = String

inputFileP :: Parser [Food]
inputFileP = foodP `endBy` newline

foodP :: Parser Food
foodP = do
  ingredients <- ingredientP `endBy` space
  string "(contains "
  allergens <- allergenP `sepBy` string ", "
  char ')'
  return (S.fromList ingredients, S.fromList allergens)

ingredientP :: Parser Ingredient
ingredientP = many letter

allergenP :: Parser Allergen
allergenP = many letter

foodContains :: Allergen -> Food -> Bool
foodContains a (is, as) = a `S.member` as

getPossibleIngredientsForAllergen :: [Food] -> Allergen -> S.Set Ingredient
getPossibleIngredientsForAllergen foodList allergen =
  let foodsContainingAllergen = L.map fst $ L.filter (foodContains allergen) foodList
   in foldl1 S.intersection foodsContainingAllergen

gatherUniqueAllergens :: [Food] -> S.Set Allergen
gatherUniqueAllergens = foldl1 S.union . L.map snd

associateAllergensToIngredients :: [Food] -> M.Map Allergen Ingredient
-- candidates for allergenes need to appear in every list where the allergen is listed
associateAllergensToIngredients foodList =
  let uniqueAllergens = gatherUniqueAllergens foodList
   in let possibleIngredients = M.fromSet (getPossibleIngredientsForAllergen foodList) uniqueAllergens
       in M.map takeSingle $ removeDuplicates possibleIngredients
  where
    removeDuplicates :: M.Map Allergen (S.Set Ingredient) -> M.Map Allergen (S.Set Ingredient)
    removeDuplicates assoc =
      -- ingredients that are unambiguously associated to an allergen
      let alreadyAssociated = L.foldl1 S.union $ M.elems $ M.filter (\s -> S.size s == 1) assoc
       in -- if its all of them, we are done
          if S.size alreadyAssociated == M.size assoc
            then assoc
            else removeDuplicates $ M.map (remove alreadyAssociated) assoc
    remove :: S.Set Ingredient -> S.Set Ingredient -> S.Set Ingredient
    remove these from =
      if S.size from == 1
        then from
        else S.foldl (flip S.delete) from these
    takeSingle :: S.Set a -> a
    takeSingle =
      takeOne . S.toList
      where
        takeOne [x] = x
        takeOne x = error "not exactly one element"

gatherAllIngredients :: [Food] -> [Ingredient]
gatherAllIngredients =
  L.concatMap (S.toList . fst)

countOccurrence :: Eq a => [a] -> a -> Int
countOccurrence is i = L.length $ L.filter (== i) is

part1 :: String -> Either ParseError Int
part1 inp = do
  foodList <- parse inputFileP "" inp
  let allIngredients = gatherAllIngredients foodList
  let allIngredientsUnique = S.fromList allIngredients
  let association = associateAllergensToIngredients foodList
  let unsafeIngredients = S.fromList $ M.elems association
  let safeIngredients = allIngredientsUnique `S.difference` unsafeIngredients
  return $ sum $ L.map (countOccurrence allIngredients) $ S.toList safeIngredients

testInput =
  unlines
    [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)"
    ]

testPart1 :: IO ()
testPart1 = assert (part1 testInput == Right 5) putStrLn "part1 worked"

part2 :: String -> Either ParseError String
part2 inp = do
  foodList <- parse inputFileP "" inp
  let association = M.elems $ associateAllergensToIngredients foodList
  -- is already sorted alphabetically
  return $ L.intercalate "," association

testPart2 :: IO ()
testPart2 = assert (part2 testInput == Right "mxmxvkd,sqjhc,fvjkl") putStrLn "part2 worked"

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
