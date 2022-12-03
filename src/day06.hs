import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.List (group, sort)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

type Coord = (Int, Int)

main :: IO ()
main = do
  input <- getInput "inputs/day06.txt"
  let coords   = map parse input
      (p1, p2) = solve coords
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parse :: String -> Coord
parse s = case words s of 
  (a:b:_) -> (read (init a), read b)
  _       -> error $ "Invalid line: " ++ s

solve :: [Coord] -> (Int, Int)
solve coords = (maximum areas, length totalDs)
  where minX = minimum (map fst coords)
        maxX = maximum (map fst coords)
        minY = minimum (map snd coords)
        maxY = maximum (map snd coords)
        internal  = [ (x, y) | x <- [minX..maxX], y <- [minY..maxY] ]
        potential = filter (\(x, y) -> x /= minX && x /= maxX && y /= minY && y /= maxY) coords
        closests  = mapMaybe (closest coords) internal
        areas     = (map length . group . sort . filter (`elem` potential)) closests
        totalDs   = filter (< 10000) $ map (totalDists coords) internal

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

totalDists :: [Coord] -> Coord -> Int
totalDists allC (x, y) = sum $ map (manhattan (x, y)) allC

closest :: [Coord] -> Coord -> Maybe Coord
closest allC (x, y) = case winners of
  []       -> Nothing
  (_:_:_)  -> Nothing
  [(c, _)] -> Just c
  where dists   = map (\c -> (c, manhattan (x, y) c)) allC
        minDist = minimum (map snd dists)
        winners = filter (\(_, d) -> d == minDist) dists
