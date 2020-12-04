import Text.ParserCombinators.Parsec
import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Exception (assert)

tr :: (Show a) => a -> a
tr x = trace (show x) x

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Passport = Map.Map String String

type Field = (String, String)

inputFileP :: GenParser Char st [Passport]
inputFileP = passportP `sepBy` string "\n\n"

passportP :: GenParser Char st Passport
passportP = do
  (k,v) <- fieldP
  Map.insert k v <$> endOrPassportP

endOrPassportP :: GenParser Char st Passport
endOrPassportP =
  ppDelim <|>
  (fieldDelim >> (passportP <|> end))
  where ppDelim = lookAhead . try $ string "\n\n" >> return mempty
        end = eof >> return mempty
        fieldDelim = oneOf "\n "

fieldP :: GenParser Char st Field
fieldP = do
  d <- descriptorP
  char ':'
  val <- valueP
  return (d, val)

descriptorP :: GenParser Char st String
descriptorP = count 3 letter

valueP :: GenParser Char st String
valueP = many (alphaNum <|> char '#')

testInput = unlines
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
   "byr:1937 iyr:2017 cid:147 hgt:183cm",
   "",
   "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
   "hcl:#cfa07d byr:1929",
   "",
   "hcl:#ae17e1 iyr:2013",
   "eyr:2024",
   "ecl:brn pid:760753108 byr:1931",
   "hgt:179cm",
   "",
   "hcl:#cfa07d eyr:2025 pid:166559648",
   "iyr:2011 ecl:brn hgt:59in"]

isValid :: Passport -> Bool
isValid pp =
  let ppFields = Map.keys pp in
  let  in
  7 == length (requiredFields `List.intersect` ppFields)

part1 :: [Passport] -> Int
part1 = length . filter isValid

testPart1 :: IO ()
testPart1 = do
  let parsed = parse inputFileP "crap" testInput
  let result = Right 2
  assert ((part1 <$> parsed) == result) return ()

heightP :: GenParser Char st (Int, String)
heightP = do
  num <- many digit
  unit <- many lower
  eof
  return (read num, unit)


haircolorP :: GenParser Char st String
haircolorP = do
  ht <- char '#'
  hex <- count 6 (digit <|> oneOf "abcdef")
  eof
  return $ ht:hex

passportIDP :: GenParser Char st String
passportIDP = read <$> do
  d <- count 9 digit
  eof
  return d

isValidStrict :: Passport -> Bool
isValidStrict pp =
  let toCheck = [isValidByr,
                 isValidIyr,
                 isValidEyr,
                 isValidHgt,
                 isValidHcl,
                 isValidEcl,
                 isValidPid] in
  let checks = zip requiredFields toCheck in
  let checked = mapM (\(k,f) -> f <$> Map.lookup k pp) checks in
  case checked of
    Nothing -> False
    (Just x) -> all (==True) x



isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween n l h = n >= l && n <= h

isValidByr :: String -> Bool
isValidIyr :: String -> Bool
isValidEyr :: String -> Bool
isValidHgt :: String -> Bool
isValidHcl :: String -> Bool
isValidEcl :: String -> Bool
isValidPid :: String -> Bool

isValidByr byr = isBetween (read byr) 1920 2002

isValidIyr iyr = isBetween (read iyr) 2010 2020

isValidEyr eyr = isBetween (read eyr) 2020 2030

isValidHgt hgt =
  let prsd = parse heightP "ups" hgt in
  case prsd of
  (Left _) -> False
  (Right (num,unit)) ->
    case unit of
    "in" -> isBetween num 59 76
    "cm" -> isBetween num 150 193
    _ -> False

isValidHcl hcl =
  let prsd = parse haircolorP "ups" hcl in
  case prsd of
  (Left _) -> False
  (Right _) -> True

isValidEcl = (`List.elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

isValidPid pid =
  case parse passportIDP "ups" pid of
  (Left _) -> False
  (Right _) -> True

part2 :: [Passport] -> Int
part2 = length . filter isValidStrict

testPart2 :: IO ()
testPart2 = do
  let testinput' = unlines ["eyr:1972 cid:100",
                    "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
                    "",
                    "iyr:2019",
                    "hcl:#602927 eyr:1967 hgt:170cm",
                    "ecl:grn pid:012533040 byr:1946",
                    "",
                    "hcl:dab227 iyr:2012",
                    "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
                    "",
                    "hgt:59cm ecl:zzz",
                    "eyr:2038 hcl:74454a iyr:2023",
                    "pid:3556412378 byr:2007"]
  let testinput'' = unlines ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
                      "hcl:#623a2f",
                      "",
                      "eyr:2029 ecl:blu cid:129 byr:1989",
                      "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
                      "",
                      "hcl:#888785",
                      "hgt:164cm byr:2001 iyr:2015 cid:88",
                      "pid:545766238 ecl:hzl",
                      "eyr:2022",
                      "",
                      "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]
  assert ((part2 <$> parse inputFileP "crap" testinput') == Right 0) $ 
    assert ((part2 <$> parse inputFileP "crap" testinput'') == Right 4)
    return ()

main = do
  testPart1
  testPart2
  input <- getContents
  print $ part1 <$> parse inputFileP "crap" input
  print $ part2 <$> parse inputFileP "crap" input
