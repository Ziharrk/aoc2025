module Day4 where

import Data.List (partition)
import qualified Data.Vector as V

import Utils ( getMatrix, setMatrix, Matrix, dir8 )

data Space = Empty | TP
  deriving Eq

day4 :: IO ()
day4 = do
  m <- V.fromList . map (V.fromList . map parse) . lines <$> readFile "input/day4"
  putStr "Part 1: "
  let (acc, idxs) = partitionAccessible [(x,y) | x <- [0 .. V.length m]
                                               , y <- [0 .. V.length (m V.! 0)]] m
      p1 = length acc
  print p1
  putStr "Part 2: "
  print (p1 + getNumAllAccessible idxs (setBlank m acc))

partitionAccessible :: [(Int, Int)] -> Matrix Space -> ([(Int, Int)], [(Int, Int)])
partitionAccessible idxs m = partition (uncurry (isAccessible m)) $
  filter (uncurry (hasTP m)) idxs

getNumAllAccessible :: [(Int, Int)] -> Matrix Space -> Int
getNumAllAccessible idxs m = case partitionAccessible idxs m of
  ([]  , _    ) -> 0
  (acc', idxs') -> length acc' + getNumAllAccessible idxs' (setBlank m acc')

isAccessible :: Matrix Space -> Int -> Int -> Bool
isAccessible m x y = 4 > length (filter (uncurry (hasTP m)) (dir8 x y))

setBlank :: Matrix Space -> [(Int, Int)] -> Matrix Space
setBlank = foldr (\(x, y) m -> setMatrix m Empty x y)

hasTP :: Matrix Space -> Int -> Int -> Bool
hasTP m x y = getMatrix m x y == Just TP

parse :: Char -> Space
parse '.' = Empty
parse '@' = TP
parse _   = error "unexpected input character"
