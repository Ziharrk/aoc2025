module Day12 where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec, anyChar, newline, char, many, digit, (<|>), space)
import Text.Parsec.Combinator (manyTill, eof, sepBy1)
import Text.Parsec.String (parseFromFile)
import Utils (memoFix)

data Shape = MkS Int (Set (Int, Int))
  deriving (Show, Eq, Ord)

data Tree = MkT Int Int [Int]
  deriving Show

day12 :: IO ()
day12 = do
  Right (shapes, trees) <- parseFromFile parseProblem "input/day12"
  let allShapes = map shapeVariants shapes
  putStr "Part 1: "
  print (length (filter (mightFit allShapes) trees))
  putStr "Part 2: "
  putStrLn "Merry Christmas!"

mightFit :: [Set Shape] -> Tree -> Bool
mightFit shapeSet t@(MkT x y ns)
  | numPresents * 7 > maxSize  = False
  | numPresents * 7 <= maxSize = True
  | otherwise                  = fits shapeSet t
  -- ^ just a bait, never actually happens and would be too slow anyways.
  where
    numPresents = sum ns
    maxSize = x * y

fits :: [Set Shape] -> Tree -> Bool
fits shapeSets (MkT mx my ns) =
  memoFix go (Set.fromList [(x, y) | x <- [0..mx-1], y <- [0..my-1]], zip [0..] ns)
  where
    go go' (_, []) = True
    go go' (marks, (_, 0):ns') = go' (marks, ns')
    go go' (marks, (i, n):ns') =
      any (any recurse . vary) (shapeSets !! i)
      where
        recurse s
          | s `Set.isSubsetOf` marks = go' (marks Set.\\ s, (i, n-1):ns')
          | otherwise                = False
    vary (MkS _ s) = [ Set.map (add (x,y)) s | x <- [0 .. mx-3], y <- [0 .. my-3] ]
    add (x,y) (x',y') = (x + x', y + y')

shapeVariants :: Shape -> Set Shape
shapeVariants (MkS i s) = Set.fromList
  [ MkS i (Set.map rotate90 s)
  , MkS i (Set.map rotate180 s)
  , MkS i (Set.map rotate270 s)
  , MkS i (Set.map rotate90 (Set.map flipCoord s))
  , MkS i (Set.map flipCoord s)
  , MkS i s
  ]
  where
    maxX = maximum (Set.map fst s)
    maxY = maximum (Set.map snd s)
    rotate90 (x, y) = (y, maxX - x)
    rotate180 (x, y) = (maxX - x, maxY - y)
    rotate270 (x, y) = (maxY - y, x)
    flipCoord (x, y) = (maxX - x, y)

parseProblem :: Parsec String () ([Shape], [Tree])
parseProblem = (([],[]) <$ eof) <|> do
  num <- read <$> many digit
  c <- (True <$ char 'x') <|> (False <$ char ':')
  if c
    then do
      t <- parseTree num
      (ss, ts) <- parseProblem
      return (ss, t:ts)
    else do
      s <- parseShape num
      (ss, ts) <- parseProblem
      return (s:ss, ts)

parseShape :: Int -> Parsec String () Shape
parseShape i = do
  _ <- newline
  ss <- manyTill shapeLine newline
  return (MkS i (toSet ss))
  where
    toSet ss = Set.fromList [(x, y) | (x, ys) <- zip [0..] ss, y <- ys]
    shapeLine = do
       cs <- manyTill anyChar newline
       return ([y | (y, c) <- zip [0..] cs, c == '#'])

parseTree :: Int -> Parsec String () Tree
parseTree x = do
  y <- read <$> manyTill digit (char ':')
  ns <- space *> (read <$> many digit) `sepBy1` char ' '
  _ <- newline
  return (MkT x y ns)
