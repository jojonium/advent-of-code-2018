import System.Environment (getArgs)

getInput :: String -> IO [Int]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (map read (words raw))

data Node = Node
  { children :: [Node]
  , metadata :: [Int]
  } deriving (Show)

main :: IO ()
main = do
  input <- getInput "inputs/day08.txt"
  let tree = fst (parse input)
  putStrLn $ "Part 1: " ++ show (totalMeta tree)
  putStrLn $ "Part 2: " ++ show (value tree)

parse :: [Int] -> (Node, [Int])
parse (0:m:xs) = (Node [] (take m xs), drop m xs)
parse (c:m:xs) = (Node childs (take m remaining), drop m remaining)
  where (childs, remaining) = foldr folder ([], xs) [1..c]
        folder _ (cs, rs)   = (cs ++ [node], rs')
          where (node, rs') = parse rs 
parse _ = error "Failed parse"

totalMeta :: Node -> Int
totalMeta (Node cs ms) = sum ms + sum (map totalMeta cs)

value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum (map helper ms)
  where helper 0 = 0
        helper i = case drop (i - 1) cs of
                   n:_ -> value n
                   []  -> 0

