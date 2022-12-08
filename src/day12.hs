import System.Environment (getArgs)
import qualified Data.Map as Map

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

type Tape = Map.Map Int Char
type Rules = Map.Map String Char

main :: IO ()
main = do
  input <- getInput "inputs/day12.txt"
  let state = toTape (drop 15 (head input))
      rules = parseRules (drop 2 input)
  putStrLn $ "Part 1: " ++ show (part1 rules state)

part1 :: Rules -> Tape -> Int
part1 rules = 
  sum . Map.keys . Map.filter (=='#') . (!! 20) . iterate (step rules)

toTape :: String -> Tape
toTape = Map.fromList . zip [0..]

parseRules :: [String] -> Rules
parseRules = Map.fromList . map parseOne
  where parseOne s = (left, right)
          where left  = take 5 s
                right = s !! 9

step :: Rules -> Tape -> Tape
step rules tape = Map.mapWithKey stepOne tape'
  where newMin      = minimum (Map.keys tape) - 2 
        newMax      = maximum (Map.keys tape) + 2 
        newPots     = [newMin .. newMax]
        tape'       = foldr (`Map.insert` '.') tape newPots
        stepOne i _ = Map.findWithDefault '.' cur rules
          where is  = [i - 2 .. i+ 2]
                cur = map (\x -> Map.findWithDefault '.' x tape) is

