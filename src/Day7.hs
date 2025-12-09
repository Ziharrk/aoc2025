module Day7 where

import qualified Data.Vector as V

import Data.List.Extra (sortOn)
import Control.Arrow (second)

import Utils (Matrix)
data Beam = Beam Integer | Empty | Split
  deriving Eq

beamNum :: Beam -> Integer
beamNum (Beam n) = n
beamNum _        = 0

day7 :: IO ()
day7 = do
  input <- V.fromList . map (V.fromList . map parse) . lines
             <$> readFile "input/day7"
  putStr "Part 1: "
  let (p1, p2) = simulate input
  print p1
  putStr "Part 2: "
  print p2

simulate :: Matrix Beam -> (Int, Integer)
simulate m = go (m V.! 0) 1 0
  where
    lm = V.length m
    go prevRow i s
      | i >= lm   = (s, sum $ fmap beamNum prevRow)
      | otherwise =
      let row = m V.! i
          rowsWith a = concatMap (\j -> [(j, n) | Beam n <- [prevRow V.! j]]) $
            V.findIndices (==a) row
          actSplit = rowsWith Split
          continuing = rowsWith Empty
          allBeams = second Beam <$> combine (concatMap neigh actSplit ++ continuing)
          row' = V.update row (V.fromList allBeams)
      in go row' (i+1) (s + length actSplit)

    neigh (n, b) = [(n-1, b), (n+1, b)]
    combine xs = dedup $ sortOn fst xs
    dedup [] = []
    dedup [x] = [x]
    dedup ((i1, n1):(i2, n2):xs)
      | i1 == i2  = dedup ((i1, n1+n2):xs)
      | otherwise = (i1, n1) : dedup ((i2, n2):xs)

parse :: Char -> Beam
parse 'S' = Beam 1
parse '.' = Empty
parse '^' = Split
parse _   = error "invalid input"
