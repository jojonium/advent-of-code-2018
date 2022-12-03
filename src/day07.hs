import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.Char (ord)
import qualified Data.Set as Set
import Data.Maybe (fromJust, fromMaybe)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

type Deps = Map.Map Char [Char]
type Task = (Char, Int) -- task, time remaining

main :: IO ()
main = do
  input <- getInput "inputs/day07.txt"
  let deps = foldr parse Map.empty input
      p1   = order deps "" ['A'..'Z']
  putStrLn $ "Part 1: " ++ p1
  putStrLn $ "Part 2: " ++ show (part2 deps Set.empty (replicate 5 (' ', 0)) (-1))

parse :: String -> Deps -> Deps
parse str = Map.insertWith (++) b [a]
  where a = str !! 5
        b = str !! 36

order :: Deps -> String -> String -> String
order _ cur [] = cur
order deps cur avail = order deps new (filter (`notElem` next) avail)
  where next = fromJust (pickNext deps cur avail)
        new  = cur ++ next

pickNext :: Deps -> String -> String -> Maybe [Char]
pickNext deps cur avail = case filter canBePicked avail of
    [] -> Nothing
    cs -> Just cs
  where canBePicked c = all (`elem` cur) (Map.findWithDefault [] c deps)

part2 :: Deps -> Set.Set Char -> [Task] -> Int -> Int
part2 _ done _ total
  | Set.size done == 26 = total
part2 deps done workers total = part2 deps done' w' (total + 1)
  where taskTime c = ord c - 5
        working  = map fst workers
        avail    = filter (\c -> c `Set.notMember` done && c `notElem` working) ['A'..'Z']
        open     = fromMaybe [] (pickNext deps (Set.toList done') avail)
        finished = filter ((== 0) . snd) workers
        done'    = foldr (Set.insert . fst) done (filter (\(c, _) -> c /= ' ') finished)
        (w', _)  = foldr folder ([], open) workers
        folder (_, 0) (ws, [])   = ((' ', 0) : ws, [])
        folder (_, 0) (ws, o:os) = ((o, taskTime o) : ws, os)
        folder (c, n) (ws, o)    = ((c, n - 1) : ws, o)
