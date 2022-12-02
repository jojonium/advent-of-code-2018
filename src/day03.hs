import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (tails)
import qualified Data.Set as Set

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return $ lines raw

type Coord = (Int, Int)
data Rect = Rect
  { _id :: Int
  , _tl :: Coord
  , _br :: Coord
  } deriving (Show, Eq)

main :: IO ()
main = do
  input <- getInput "inputs/day03.txt"
  let claims = map parse input
  putStrLn $ "Part 1: " ++ show (part1 claims)
  putStrLn $ "Part 2: " ++ show (part2 claims)

parse :: String -> Rect
parse s = let nums = getAllTextMatches (s =~ "[0-9]+") :: [String]
  in case nums of
     (i:x:y:w:h:_) -> let o1 = (read x, read y)
                          o2 = (read x + read w - 1, read y + read h - 1)
                      in Rect (read i) o1 o2
     _             -> error "Parse error"

overlap :: Rect -> Rect -> Maybe Rect
overlap a b
  | bry a < tly b = Nothing  -- a above b
  | bry b < tly a = Nothing  -- b above a
  | brx a < tlx b = Nothing  -- a left of b
  | brx b < tlx a = Nothing  -- b left of a
  | otherwise = Just (Rect (-1) o1 o2)
  where tlx = fst . _tl
        tly = snd . _tl
        brx = fst . _br
        bry = snd . _br
        o1  = (max (tlx a) (tlx b), max (tly a) (tly b))
        o2  = (min (brx a) (brx b), min (bry a) (bry b))

allPoints :: Rect -> Set.Set Coord
allPoints (Rect _ (tlx, tly) (brx, bry)) = foldr Set.insert Set.empty coords
  where coords = [ (x, y) | x <- [tlx..brx], y <- [tly..bry] ]

part1 :: [Rect] -> Int
part1 rects = Set.size (foldr (Set.union . allPoints) Set.empty sets)
  where sets = catMaybes [ overlap a b | (a:bs) <- tails rects, b <- bs ]

part2 :: [Rect] -> Int
part2 rects = fst $ head $ filter (\(_, xs) -> length xs == 1) olaps
  where olaps = map (\r -> (_id r, mapMaybe (overlap r) rects)) rects

