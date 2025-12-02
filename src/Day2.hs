module Day2 where

import Data.List.Extra (split)
import Utils (parMap, splitAtInterval)

day2 :: IO ()
day2 = do
  input <- map parse . split (==',') <$> readFile "input/day2"
  putStr "Part 1: "
  res1 <- parMap (invalidIDs isInterestingP1) input
  print (sum $ concat res1)
  putStr "Part 2: "
  res2 <- parMap (invalidIDs isInterestingP2) input
  print (sum $ concat res2)

invalidIDs :: (String -> Bool) -> (Int, Int) -> [Int]
invalidIDs f = filter (f . show) . uncurry enumFromTo

isInterestingP1 :: String -> Bool
isInterestingP1 s
  | even sl   = s `isInterestingOn` (sl `div` 2)
  | otherwise = False
  where
    sl = length s

isInterestingP2 :: String -> Bool
isInterestingP2 s = any (s `isInterestingOn`) $ filter divisible [1 .. sl-1]
  where
    divisible n = sl `mod` n == 0
    sl = length s

isInterestingOn :: String -> Int -> Bool
isInterestingOn s n = case ss of
  []     -> False
  (x:xs) -> all (==x) xs
  where
    ss = splitAtInterval n s

parse :: String -> (Int, Int)
parse s = case split (=='-') s of
  [s1, s2] -> (read s1, read s2)
  _ -> error ""
