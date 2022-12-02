import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.List (sort, foldl', maximumBy)
import qualified Data.Map as Map

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return $ sort (lines raw)

data Time = Time
  { _mon :: Int
  , _day :: Int
  , _hr  :: Int
  , _min :: Int
  } deriving (Show, Eq)
data Event = Shift Int | FallAsleep | WakeUp deriving (Show)
type SleepPeriod = (Time, Time)
type Log = Map.Map Int [SleepPeriod]

main :: IO ()
main = do
  input <- getInput "inputs/day04.txt"
  let sleepLog = collate (map parse input)
  putStrLn $ "Part 1: " ++ show (part1 sleepLog)
  putStrLn $ "Part 2: " ++ show (part2 sleepLog)

parse :: String -> (Time, Event)
parse l = (Time mon day hr mn, evt)
  where mon = read (take 2 (drop 6 l))
        day = read (take 2 (drop 9 l))
        hr  = read (take 2 (drop 12 l))
        mn  = read (take 2 (drop 15 l))
        evt = case drop 19 l of
                'f':_ -> FallAsleep
                'w':_ -> WakeUp
                'G':_ -> let n = read (drop 19 l =~ "[0-9]+")
                         in Shift n
                _     -> error "Parse error"

collate :: [(Time, Event)] -> Log
collate = fst . foldl' process (Map.empty, 0)

process :: (Log, Int) -> (Time, Event) -> (Log, Int)
process (l, _) (_, Shift n)    = (l, n)
process (l, n) (t, FallAsleep) = (Map.insertWith (++) n [(t, t)] l, n)
process (l, n) (t, WakeUp)     = (Map.insert n periods' l, n)
  where periods  = l Map.! n
        lastP    = (fst (head periods), t)
        periods' = lastP : tail periods

totalMinutesAsleep :: [SleepPeriod] -> Int
totalMinutesAsleep = sum . map countMins
  where countMins (t1, t2) = hours * 60 + minutes
          where hours   = (_hr t2 + 24) - (_hr t1 + 24)
                minutes = _min t2 - _min t1

mostCommonMinute :: [SleepPeriod] -> (Int, Int)
mostCommonMinute periods = maximumBy comp $ zip [0..59] (map timesSeen [0..59])
  where comp (_, a) (_, b) = a `compare` b
        timesSeen x        = length (filter within periods)
          where within (t1, t2) =  (x >= _min t1) && (x < _min t2)

part1 :: Log -> Int
part1 sleepLog = guard * mcm
  where totals = Map.map totalMinutesAsleep sleepLog
        guard  = fst $ maximumBy (\(_, a) (_, b) -> a `compare` b) (Map.toList totals)
        mcm    = snd $ mostCommonMinute (sleepLog Map.! guard)

part2 :: Log -> Int
part2 sleepLog = wk * wm
  where mcms        = Map.map mostCommonMinute sleepLog
        (wk, wm, _) = Map.foldrWithKey findWin (0, 0, 0) mcms
        findWin k (m, c) (bestk, bestm, bestc)
          | c > bestc = (k, m, c)
          | otherwise = (bestk, bestm, bestc)
