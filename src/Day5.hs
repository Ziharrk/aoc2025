module Day5 where

day5 :: IO ()
day5 = do
  (inputR, inputIDs) <- break null . lines <$> readFile "input/day5"
  let ranges = foldr (addRange . toRange) [] inputR
      ids = map read $ drop 1 inputIDs
  putStr "Part 1: "
  let fresh = filter (inRanges ranges) ids
  print (length fresh)
  putStr "Part 2: "
  print (sum $ map rangeSize ranges)

toRange :: String -> (Integer, Integer)
toRange s = (read l, read $ drop 1 h)
  where
    (l, h) = break (=='-') s

addRange :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
addRange (l, h) [] = [(l, h)]
addRange (l, h) rs@((l', h'):rs')
  | h < l'    = (l , h ) : rs
  | l > h'    = (l', h') : addRange (l, h) rs'
  | otherwise = addRange (min l l', max h h') rs'

inRanges :: [(Integer, Integer)] -> Integer -> Bool
inRanges rs i = any (\(l, h) -> l <= i && i <= h) rs

rangeSize :: (Integer, Integer) -> Integer
rangeSize (l, h) = h-l+1
