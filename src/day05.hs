import System.Environment (getArgs)
import Data.Char (ord, toUpper)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day05.txt"
  let reduced = full input
  putStrLn $ "Part 1: " ++ show (length reduced)
  putStrLn $ "Part 2: " ++ show (part2 reduced)

full :: String -> String
full s
  | next == s = next
  | otherwise = full next
  where next = reduce s

reduce :: String -> String
reduce (a:b:cs)
  | abs (ord a - ord b) == 32 = reduce cs
  | otherwise                 = a : reduce (b : cs)
reduce s = s

without :: Char -> String -> String
without x = filter (\c -> c /= x && c /= toUpper x)

part2 :: String -> Int
part2 red = minimum $ map (\x -> length (full (without x red))) ['a'..'z']
