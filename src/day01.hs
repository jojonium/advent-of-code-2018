import System.Environment (getArgs)
import qualified Data.Set as Set

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day01.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Integer
parse ('+':x) = read x
parse str     = read str

part1 :: String -> Integer
part1 = sum . map parse . lines

part2 :: String -> Integer
part2 = firstDup Set.empty . scanl (+) 0 . map parse . cycle . lines
  where firstDup s (x:xs)
          | x `Set.member` s = x
          | otherwise        = firstDup (Set.insert x s) xs
        firstDup s [] = error "No solution"
