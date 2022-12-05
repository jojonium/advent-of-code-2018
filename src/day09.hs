import System.Environment (getArgs)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Map as Map
import Debug.Trace (trace)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day09.txt"
  let ws = words input
      numPlayers = read (head ws)
      maxPoints  = read (ws !! 6)
  putStrLn $ "Part 1: " ++ show (part1 numPlayers maxPoints)
  putStrLn $ "Part 2 (this will take a while): " ++ show (part1 numPlayers (maxPoints * 100))

part1 :: Int -> Int -> Int
part1 numPlayers maxPoints = 
  let (s, _) = foldl' folder (Map.empty, PL.singleton 0) (zip players marbles)
  in maximum (Map.elems s)
  where marbles = [1..maxPoints]
        players = (`mod` numPlayers) <$> [0..]
        folder (ss, tp) (p, i) = (scores', tp')
          where (pts, tp') = play i tp
                scores'    = Map.insertWith (+) p pts ss

play :: Int -> PL.PointedList Int -> (Int, PL.PointedList Int)
play marble circle
  | marble `mod` 23 == 0 = 
    let c'    = PL.moveN (-7) circle
        c''   = fromJust (PL.deleteRight c')
        score = marble + PL._focus c'
    in (score, c'')
  | otherwise            = 
    let c'  = PL.moveN 2 circle
        c'' = PL.insertLeft marble c'
    in (0, c'')
