import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.List (intercalate)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

data Star = Star
  { _px :: Int
  , _py :: Int
  , _vx :: Int
  , _vy :: Int
  } deriving (Show)

main :: IO ()
main = do
  input <- getInput "inputs/day10.txt"
  let stars = map parse input
      (message, steps) = align maxBound 0 stars
  putStrLn "Part 1:"
  putStrLn $ prettyPrint message
  putStrLn $ "Part 2: " ++ show steps

parse :: String -> Star
parse s = case getAllTextMatches (s =~ "[0-9-]+") :: [String] of
  (a:b:c:d:_) -> Star (read a) (read b) (read c) (read d)
  _           -> error "Failed to parse"

-- stars are aligned when the area they take up is at a minimum
align :: Int -> Int -> [Star] -> ([Star], Int)
align prevArea n stars
  | newArea > prevArea = (stars, n)
  | otherwise          = align newArea (n + 1) stars'
  where stars'  = step stars
        newArea = area stars'

area :: [Star] -> Int
area stars = (maxx - minx) * (maxy - miny)
  where xs   = map _px stars
        ys   = map _py stars
        minx = minimum xs
        maxx = maximum xs
        miny = minimum ys
        maxy = maximum ys

prettyPrint :: [Star] -> String
prettyPrint stars = intercalate "\n" [ makeLine y | y <- [miny .. maxy] ]
  where xs   = map _px stars
        ys   = map _py stars
        minx = minimum xs
        maxx = maximum xs
        miny = minimum ys
        maxy = maximum ys
        makeLine y = map (makeChar y) [minx..maxx]
        makeChar y x = case filter (\s -> _px s == x && _py s == y) stars of
                       (_:_) -> '#'
                       _     -> ' '

step :: [Star] -> [Star]
step = map stepOne
  where stepOne (Star px py vx vy) = Star (px + vx) (py + vy) vx vy
