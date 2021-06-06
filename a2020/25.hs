import Control.Exception (assert)
import qualified Data.List as L
import Data.Maybe (fromJust)

initSNum = 7 :: Int

modNum = 20201227 :: Int

encryptStep ::
  Int -> -- subject number
  Int -> -- value
  Int -- encrypted number
encryptStep sNum value = (value * sNum) `mod` modNum

findLS :: Int -> Int
findLS target = fromJust $ L.elemIndex target $ iterate (encryptStep initSNum) 1

applyN :: Int -> (a -> a) -> a -> a
applyN n f x = iterate f x !! n

encrypt ::
  Int -> -- n times
  Int -> -- subject number
  Int -- encrypted number
encrypt n s = applyN n (encryptStep s) 1

testInput :: String
testInput = unlines ["5764801", "17807724"]

part1 :: String -> Int
part1 inp = encKey
  where
    encKey = encrypt doorLS cardPK
    [cardPK, doorPK] = map read (lines inp) :: [Int]
    doorLS = findLS doorPK

testPart1 :: IO ()
testPart1 = assert (part1 testInput == 14897079) putStrLn "part1 worked"

main :: IO ()
main = do
  testPart1
  input <- getContents
  print $ part1 input
