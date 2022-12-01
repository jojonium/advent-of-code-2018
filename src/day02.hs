import System.Environment (getArgs)
import Data.List (group, sort)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw <- readFile (case args of [] -> defFile; x:_ -> x)
  return $ lines raw

main :: IO ()
main = do
  input <- getInput "inputs/day02.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ part2 input

hasN :: Int -> String -> Bool
hasN n = any (\x -> length x == n) . group . sort

part1 :: [String] -> Int
part1 strs = doubles * triples
  where doubles = length (filter (hasN 2) strs)
        triples = length (filter (hasN 3) strs)

part2 :: [String] -> String
part2 strings = head $ filter (\s -> length s == target) commons
  where target  = length (head strings) - 1
        commons = [ inCommon a b | a <- strings, b <- strings ]

inCommon :: String -> String -> String
inCommon x = map fst . filter (uncurry (==)) . zip x
