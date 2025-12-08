module Day8 where

import Control.Monad (unless, void)
import Control.Monad.State (execState, put)
import Data.Equivalence.Monad (MonadEquiv(..), runEquivM, runEquivT')
import Data.List (sortBy, sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (comparing, Down (..))
import qualified Data.Set as Set
import Data.Tuple.Extra (thd3)

import Utils (allPairs, iterateNM, untilAll)

type C3 = (Int, Int, Int)

day8 :: IO ()
day8 = do
  input <- map (triple . map read . splitOn ",") . lines <$> readFile "input/day8"
  let withDists = sortOn thd3 [ (e1, e2, d) | (e1, e2) <- allPairs input
                                            , let d = distSq3 e1 e2]
  putStr "Part 1: "
  let eqs = runEquivM Set.singleton Set.union $
              iterateNM 1000 (addToEquivalence return) withDists
              >> classes >>= mapM desc -- extract equivalence classes
      sortedEqs = take 3 $ sortBy (comparing Down) (map Set.size eqs)
  print (product sortedEqs)
  putStr "Part 2: "
  let ((x1,_,_), (x2,_,_)) = flip execState (error "everything already equivalent") $
        runEquivT' $ untilAll (addToEquivalence put) withDists
  print (x1 * x2)

{-# INLINE addToEquivalence #-}  -- ^ saves ~0.5s due to specialization on type of m
addToEquivalence :: MonadEquiv c C3 d m
                 => ((C3, C3) -> m a) -- ^ action to execute for already equivalent values
                 -> [(C3, C3, Int)]   -- ^ sorted list of point pairs and their distances
                 -> m [(C3, C3, Int)]
addToEquivalence _ [] = return []
addToEquivalence act ((x,y,_):xs) = do
  b <- equivalent x y
  unless b $ void $ act (x, y)
  equate x y
  return xs

triple :: [Int] -> C3
triple [x, y, z] = (x, y, z)
triple _         = error "invalid input"

distSq3 :: C3 -> C3 -> Int
distSq3 (x1, y1, z1) (x2, y2, z2) = sq (x1-x2) + sq (y1-y2) + sq (z1-z2)
  where sq = (^(2::Int))
