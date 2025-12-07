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
  let (p1, p2, _) = simulate input
  print p1
  putStr "Part 2: "
  print p2

simulate :: Matrix Beam -> (Int, Integer, Matrix Beam)
simulate m'' = go m'' 1 0
  where
    lm = V.length m''
    go m i s
      | i >= lm    = (s, sum $ fmap beamNum (m V.! (i-1)), m)
      | otherwise =
      let row = m V.! i
          prevRow = m V.! (i-1)
          rowsWith a = concatMap (\j -> [(j, n) | Beam n <- [prevRow V.! j]]) $
            V.findIndices (==a) row
          actSplit = rowsWith Split
          continuing = rowsWith Empty
          allBeams = second Beam <$> combine (concatMap neigh actSplit ++ continuing)
          row' = V.update row (V.fromList allBeams)
          m' = V.update m (V.fromList [(i, row')])
      in go m' (i+1) (s + length actSplit)

    neigh (n, b) = [(n-1, b), (n+1, b)]
    combine xs = dedup $ sortOn fst xs
    dedup [] = []
    dedup [x] = [x]
    dedup ((i, n):(j, m):xs)
      | i == j = dedup ((i, n+m):xs)
      | otherwise = (i, n) : dedup ((j, m):xs)

parse :: Char -> Beam
parse 'S' = Beam 1
parse '.' = Empty
parse '^' = Split
parse _   = error "invalid input"
