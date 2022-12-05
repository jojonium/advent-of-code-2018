import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)

getInput :: String -> IO Int
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (read raw)

main :: IO ()
main = do
  input <- getInput "inputs/day11.txt"
  let (p1x, p1y, _) = part1 input
  putStrLn $ "Part 1: " ++ show p1x ++ "," ++ show p1y
  let (p2x, p2y, p2d) = part2 input
  putStrLn $ "Part 2: " ++ show p2x ++ "," ++ show p2y ++ "," ++ show p2d

cellPower :: Int -> (Int, Int) -> Int
cellPower serialNum (x, y) = ((power `div` 100) `mod` 10) - 5
  where rackId = x + 10
        power  = (rackId * y + serialNum) * rackId

squarePower :: Int -> (Int, Int, Int) -> Int
squarePower serialNum (x, y, dim) = sum (map (cellPower serialNum) members)
  where members = [ (x + dx, y + dy) | dx <- [0..(dim - 1)], dy <- [0..(dim - 1)] ]

part1 :: Int -> (Int, Int, Int)
part1 serialNum = maximumBy (comparing (squarePower serialNum)) toTest
  where toTest  = [ (x, y, 3) | x <- [1..298], y <- [1..298] ]

part2 :: Int -> (Int, Int, Int)
part2 serialNum = maximumBy (comparing (squarePower serialNum)) toTest
  where toTest  = [ (x, y, d) | 
                     d <- [3..300],
                     x <- [1..(300 - d - 1)], y <- [1..(300 - d - 1)]
                  ]
