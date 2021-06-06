import Control.Monad (guard)
import Control.Exception (assert)
import Safe (headMay)
import Data.Maybe (isNothing)

part1 :: [Int] -> Maybe Int
part1 xs = headMay $ do
  xi <- xs
  xj <- xs
  guard $ xi + xj == 2020
  guard $ xi /= xj
  return $ xi*xj

test_part1 :: IO()
test_part1 = do
  let testSucc = [200,1800,1620,400,2010] :: [Int]
  let testFail = [1,2,3,4,5] :: [Int]
  assert (part1 testSucc == Just 648000) $
    assert (isNothing $ part1 testFail) $
    return ()

part2 :: [Int] -> Maybe Int
part2 xs = headMay $ do
  xi <- xs
  xj <- xs
  xk <- xs
  guard $ xi + xj + xk == 2020
  guard $ (xi /= xj) && (xi /= xk) && (xj /= xk)
  return $ xi*xj*xk

test_part2 :: IO()
test_part2 = do
  let testSucc = [200,1800,1620,400,2010,1420] :: [Int]
  let testFail = [1,2,3,4,5] :: [Int]
  assert (part2 testSucc == Just 113600000) $
    assert (isNothing $ part2 testFail) $
    return ()

getInput :: IO [Int]
getInput = map read . lines <$> getContents

main :: IO()
main = do
  input <- getInput
  test_part1
  print $ part1 input
  test_part2
  print $ part2 input
