module Day3 where

day3 :: IO ()
day3 = do
  input <- lines <$> readFile "input/day3"
  putStr "Part 1: "
  print (sum (map (read @Int . maxJoltsWith  2) input))
  putStr "Part 2: "
  print (sum (map (read @Int . maxJoltsWith 12) input))

maxJoltsWith :: Int -> [Char] -> [Char]
maxJoltsWith 0 _  = []
maxJoltsWith k xs = m : maxJoltsWith k' xs'
  where
    k' = k-1
    m = maximum (take (length xs - k') xs)
    xs' = drop 1 $ dropWhile (/= m) xs
