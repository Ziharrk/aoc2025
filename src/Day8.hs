module Day8 where

import Control.Monad (unless, void)
import Control.Monad.State (execState, put)
import Data.Equivalence.Monad (MonadEquiv(..), runEquivM, runEquivT')
import Data.List (sortBy, sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (comparing, Down (..))
import qualified Data.Set as Set

import Utils (allPairs)

type C3 = (Int, Int, Int)

day8 :: IO ()
day8 = do
  input <- map (triple . map read . splitOn ",") . lines <$> readFile "input/day8"
  let sorted = sortOn (uncurry distSq3) (allPairs input)
  putStr "Part 1: "
  let eqs = runEquivM Set.singleton Set.union $
              mapM_ (addToEquivalence return) (take 1000 sorted)
              >> classes >>= mapM desc -- extract equivalence classes
      sortedEqs = take 3 $ sortBy (comparing Down) (map Set.size eqs)
  print (product sortedEqs)
  putStr "Part 2: "
  let ((x1,_,_), (x2,_,_)) = flip execState (error "everything already equivalent") $
        runEquivT' $ mapM_ (addToEquivalence put) (take 10000 sorted)
  print (x1 * x2)

addToEquivalence :: MonadEquiv c C3 d m
                 => ((C3, C3) -> m a) -- ^ action to execute for already equivalent values
                 -> (C3, C3)
                 -> m ()
addToEquivalence act (c1, c2) = do
  b <- equivalent c1 c2
  unless b $ void $ act (c1, c2)
  equate c1 c2

triple :: [Int] -> C3
triple [x, y, z] = (x, y, z)
triple _         = error "invalid input"

distSq3 :: C3 -> C3 -> Int
distSq3 (x1, y1, z1) (x2, y2, z2) = sq (x1-x2) + sq (y1-y2) + sq (z1-z2)
  where sq x = x * x
