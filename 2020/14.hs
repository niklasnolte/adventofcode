import Data.Bits ((.&.), (.|.))
import Control.Exception(assert)
import Data.Functor(($>))
import qualified Data.Map as MAP
import Text.ParserCombinators.Parsec


data MaskUnit = Zero | One | Identity deriving (Eq, Show, Enum)

type Mask = [MaskUnit]
type Assignment = (Int, Int)
type Instr = (Mask, Assignment)
type Memory = MAP.Map Int Int

inputFileP :: Parser [Instr]
inputFileP = concat <$> many (do
  mask <- maskP
  char '\n'
  assignments <- try assignmentP `endBy` char '\n'
  return [(mask, a) | a <- assignments]
  )

maskP :: Parser Mask
maskP = do
  string "mask = "
  many $
    choice [char 'X' $> Identity,
            char '1' $> One,
            char '0' $> Zero]

assignmentP :: Parser Assignment
assignmentP = do
  string "mem["
  addr <- read <$> many digit
  string "] = "
  n <- read <$> many digit
  return (addr, n)

toDec :: [Int] -> Int
toDec = foldl (\acc x -> acc * 2 + x) 0

toBin :: Int -> [Int]
toBin n = reverse $ take 36 (helper n ++ repeat 0)
  where
    helper :: Int -> [Int]
    helper 0 = []
    helper n = let (q,r) = n `divMod` 2 in r : helper q

applyMask :: Mask -> Int -> Int
applyMask m i =
  let (m1,m2) = unzip $ map convert m in
  toDec m2 .&. (toDec m1 .|. i)
  where
    convert :: MaskUnit -> (Int, Int)
    convert One = (1,1)
    convert Zero = (0,0)
    convert Identity = (0,1)

runInstr :: Memory -> Instr -> Memory
runInstr mem (mask, (ptr, n)) = MAP.insert ptr (applyMask mask n) mem

runAllInstrs :: [Instr] -> Memory
runAllInstrs = foldl runInstr mempty

applyMaskToMemAddress :: Mask -> Int -> [Int]
applyMaskToMemAddress mask ptr =
  let maskedPtr = applyPtrMask mask (toBin ptr) in
  expand maskedPtr
  where
    applyPtrMask :: Mask -> [Int] -> Mask
    applyPtrMask mask binI = zipWith applyOne mask binI
    applyOne :: MaskUnit -> Int -> MaskUnit
    applyOne mu i = case mu of
      One -> One
      Zero -> toEnum i
      Identity -> Identity
    expand :: Mask -> [Int]
    expand x = toDec <$> mapM convertX x
    convertX :: MaskUnit -> [Int]
    convertX One = [1]
    convertX Zero = [0]
    convertX Identity = [0,1]

runInstrV2 :: Memory -> Instr -> Memory
runInstrV2 mem (mask, (ptr, n)) =
  let ptrs = applyMaskToMemAddress mask ptr in
  foldl (\mem' ptr -> MAP.insert ptr n mem') mem ptrs

runAllInstrsV2 :: [Instr] -> Memory
runAllInstrsV2 = foldl runInstrV2 mempty

partX :: ([Instr] -> Memory) -> String -> Either ParseError Int
partX runInstrs inp = do
  instrs <- parse inputFileP "" inp
  let mem = runInstrs instrs
  return $ sum mem

part1 = partX runAllInstrs
part2 = partX runAllInstrsV2

testInput = unlines
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
   "mem[8] = 11",
   "mem[7] = 101",
   "mem[8] = 0"]

testInputP2 = unlines
  ["mask = 000000000000000000000000000000X1001X",
   "mem[42] = 100",
   "mask = 00000000000000000000000000000000X0XX",
   "mem[26] = 1"]

testParse :: IO ()
testParse =
  assert ((length <$> parse inputFileP "" testInput) == Right 3) $ putStrLn "parsing worked" 

testPart1 :: IO ()
testPart1 =
  assert (part1 testInput == Right 165) $ putStrLn "part1 worked"

testPart2 :: IO ()
testPart2 =
  assert (part2 testInputP2 == Right 208) $ putStrLn "part2 worked"


main = do
  testParse
  testPart1
  testPart2
  input <- getContents
  print $ part1 input
  print $ part2 input
