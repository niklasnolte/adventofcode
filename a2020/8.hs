import Safe(headMay)
import Control.Monad(unless)
import Control.Exception (assert)
import qualified Control.Monad.State as ST
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable(write)
import qualified Data.HashSet as Set
import Text.Read (readMaybe)

data InstrType = Nop | Acc | Jmp deriving (Enum, Show)
type Instr = (InstrType, Int)
type History = Set.HashSet Int
type Program = V.Vector Instr

toInstrType :: String -> Maybe InstrType
toInstrType "nop" = Just Nop
toInstrType "acc" = Just Acc
toInstrType "jmp" = Just Jmp
toInstrType _ = Nothing

parseInstrs :: String -> Maybe Program
parseInstrs = sequence . V.fromList . map parseInstr . lines

parseInstr :: String -> Maybe Instr
parseInstr inp = do
  instrt <- toInstrType $ take 3 inp
  sign <- case inp !! 4 of
          '+' -> Just 1
          '-' -> Just (-1)
          _ -> Nothing
  n <- readMaybe (drop 5 inp) :: Maybe Int
  return (instrt, n*sign)

data ComState = Running | Terminated deriving (Enum, Show, Eq)
data Computer = Computer { getProg :: Program,
                           getAcc :: Int,
                           getInstrPtr :: Int,
                           getState :: ComState
                         } deriving (Show)

runNextInstr :: ST.State Computer ()
runNextInstr = do
  c@(Computer p ac iptr _) <- ST.get
  let (instrT, n) = p V.! iptr
  let c' = case instrT of
            Nop -> c{getInstrPtr = iptr+1}
            Acc -> c{getInstrPtr = iptr+1,
                     getAcc = ac + n}
            Jmp -> c{getInstrPtr = iptr+n}
  if getInstrPtr c' < V.length p
    then ST.put c'
    else ST.put c'{getState = Terminated}

runUntilLoopOrTermination :: ST.State (Computer, History) ()
runUntilLoopOrTermination = do
  (c,hist) <- ST.get
  unless (getState c == Terminated) $ continue c hist
  where
    continue c hist = do
      let c' = ST.execState runNextInstr c
      let cPtr = getInstrPtr c'
      unless (Set.member cPtr hist) $ do
        let hist' = Set.insert cPtr hist
        ST.put (c', hist')
        runUntilLoopOrTermination

part1 :: String -> Maybe Int
part1 inp = do
  prog <- parseInstrs inp
  let com = Computer prog 0 0 Running
  let (com',_) = ST.execState runUntilLoopOrTermination (com,mempty)
  case getState com' of
    -- for the prog to contain a loop, the com should not terminate
    Terminated -> Nothing
    -- Running is fine
    Running -> Just $ getAcc com'

flipOpAt :: Int -> Program -> Program
flipOpAt i prog =
  let (op,n) = prog V.! i in
  let newop = case op of
              Jmp -> Nop
              Nop -> Jmp
              Acc -> Acc
  in V.modify (\mw -> write mw i (newop,n)) prog

modifyUntilTermination :: Computer -> Maybe Computer
modifyUntilTermination initCom =
  let (_, visitedInstrs) = run initCom in
  let coms = do
      instrToChange <- Set.toList visitedInstrs
      let modifiedProg = flipOpAt instrToChange $ getProg initCom
      let com = initCom{getProg = modifiedProg}
      return $ fst $ run com
  in headMay $ filter didTerminate coms
  where
    run :: Computer -> (Computer, History)
    run c = ST.execState runUntilLoopOrTermination (c, mempty)
    didTerminate = (==Terminated) . getState

part2 :: String -> Maybe Int
part2 inp = do
  prog <- parseInstrs inp
  getAcc <$> modifyUntilTermination (Computer prog 0 0 Running)

testInput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

testPart1 :: IO ()
testPart1 = do
  assert (part1 testInput == Just 5) $ print "part1 worked"


testPart2 :: IO ()
testPart2 = do
  assert (part2 testInput == Just 8) $ print "part2 worked"


main = do
  testPart1
  testPart2
  inp <- getContents
  print $ part1 inp
  print $ part2 inp
