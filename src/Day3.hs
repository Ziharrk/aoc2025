module Day3 where

day3 :: IO ()
day3 = do
  input <- map (map (read . return)) . lines <$> readFile "input/day3"
  putStr "Part 1: "
  print (sum (map (fromList . maxJoltsWith  2) input))
  putStr "Part 2: "
  print (sum (map (fromList . maxJoltsWith 12) input))

maxJoltsWith :: Int -> [Integer] -> [Integer]
maxJoltsWith 0 _  = []
maxJoltsWith k xs = m : maxJoltsWith k' xs'
  where
    k' = k-1
    m = maximum (dropBack k' xs)
    xs' = drop 1 $ dropWhile (/= m) xs

fromList :: [Integer] -> Integer
fromList = sum . zipWith (\e n -> 10^e * n) [(0::Int)..] . reverse

dropBack :: Int -> [a] -> [a]
dropBack n = reverse . drop n . reverse
