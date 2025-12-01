module Day1 where

import Data.List (scanl')

data Dir = L | R

day1 :: IO ()
day1 = do
  input <- map parse . lines <$> readFile "input/day1"
  let simulated = scanl' simulate (50, 0) input
  putStr "Part 1: "
  print (length $ filter ((==0) . fst) simulated)
  putStr "Part 2: "
  print (snd $ last simulated)

parse :: String -> (Dir, Int)
parse ('L':xs) = (L, read xs)
parse ('R':xs) = (R, read xs)
parse _ = error "Wrong input format"

simulate :: (Int, Int) -> (Dir, Int) -> (Int, Int)
simulate (0, z) (L, n) = ((- n) `mod` 100, z + n `div` 100)
simulate (curr, z) (L, n) = (m, z + abs d + if m == 0 then 1 else 0)
  where (d, m) = divMod (curr - n) 100
simulate (curr, z) (R, n) = (m, z + abs d)
  where (d, m) = divMod (curr + n) 100
