module Day11 where

import Control.Arrow (second)
import qualified Data.Map as Map

import Utils (memoFix)

day11 :: IO ()
day11 = do
  input <- Map.fromList . map (second (words . drop 2) . break (== ':')) . lines <$> readFile "input/day11"
  putStr "Part 1: "
  print (pathsToVia input "you" [] "out")
  putStr "Part 2: "
  print (pathsToVia input "svr" ["fft", "dac"] "out")

pathsToVia :: Map.Map String [String] -> String -> [String] -> String -> Int
pathsToVia graph s v end = memoFix go (s, v)
  where
    go pathsToVia' (start, vias)
      | start == end = if null vias then 1 else 0
      | otherwise =
        sum $ map (\n -> pathsToVia' (n, filter (/= n) vias)) $
        Map.findWithDefault [] start graph
