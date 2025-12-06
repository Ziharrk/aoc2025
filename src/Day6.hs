module Day6 where

import Data.List (unsnoc, transpose)
import Data.List.Extra (splitOn)
import Data.Maybe (isNothing)

day6 :: IO ()
day6 = do
  Just (numberL, opL) <- unsnoc . lines <$> readFile "input/day6"
  let ops = map parse $ words opL
  putStr "Part 1: "
  let numbersP1 = transpose $ map (map read. words) numberL
  print (sum $ zipWith ($) ops numbersP1)
  putStr "Part 2: "
  let numbersP2 = parseNumCols numberL
  print (sum $ zipWith ($) ops numbersP2)

parse :: String -> [Integer] -> Integer
parse "+" = sum
parse "*" = product
parse _   = error "malformed input"

parseNumCols :: [String] -> [[Integer]]
parseNumCols = map (map toNum) . splitOn [[]] . parseColumnwise
  where
    parseColumnwise xs
      | all null xs = []
      | otherwise   =
        let ps = map (toDigit . take 1) xs
        in (if all isNothing ps then [] else ps)
             : parseColumnwise (map (drop 1) xs)
    toDigit " " = Nothing
    toDigit ""  = Nothing
    toDigit s   = Just (read s)
    toNum = foldl go 0
    go n Nothing  = n
    go n (Just d) = 10 * n + d
