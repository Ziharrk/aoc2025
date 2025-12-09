module Day9 where

import Control.Arrow (second)
import Data.List (sortOn)
import Data.Tuple.Extra (both)

import Utils (allPairs)

day9 :: IO ()
day9 = do
  input <- map (both read . second (drop 1) . break (==',')). lines
            <$> readFile "input/day9"
  putStr "Part 1: "
  let (allRects, maxRect) = case sortOn (negate . uncurry area) $ allPairs input of
        (x:xs) -> (x:xs, x)
        []     -> error "No rectangles found"
  print (uncurry area maxRect)
  putStr "Part 2: "
  let bestRect = case filter (uncurry (inBounds input)) allRects of
        (x:_) -> x
        []    -> error "No valid rectangle found"
  print (uncurry area bestRect)

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1-x2) + 1) * (abs (y1-y2) + 1)

inBounds :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
inBounds input (rx1, ry1) (rx2, ry2) = all (uncurry isOk) $
  zip input (drop 1 (cycle input))
  where
    (xMin, xMax) = (min rx1 rx2, max rx1 rx2)
    (yMin, yMax) = (min ry1 ry2, max ry1 ry2)
    isOk (x1, y1) (x2, y2) = xMin >= max x1 x2
      || xMax <= min x1 x2
      || yMin >= min y1 y2
      || yMax <= max y1 y2
