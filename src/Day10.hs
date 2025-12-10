module Day10 where

import Control.Monad (zipWithM_)
import Data.SBV
  ( OptimizeResult (..), OptimizeStyle (..)
  , SymVal, SBV, getModelValue
  , minimize, optimize, constrain, literal, symbolic
  , fromSDouble, sRoundTowardZero
  , (.>=), (.==), sMod)
import Text.Parsec
    ( Parsec
    , char, digit, newline, spaces, eof
    , many1, sepBy1, sepEndBy1, (<|>)
    )
import Text.Parsec.String (parseFromFile)

import Utils (parMapM)

data Machine = MkM { states :: [Bool], buttons :: [[Int]], joltages :: [Double] }
  deriving Show

day10 :: IO ()
day10 = do
  Right input <- parseFromFile (many1 parseMachine <* eof) "input/day10"
  putStr "Part 1: "
  let oddPresses xs = 1 .== (sum (map (fromSDouble sRoundTowardZero) xs) `sMod` 2)
  res1 <- parMapM (fewestPresses oddPresses states) input
  print (sum res1)
  putStr "Part 2: "
  res2 <- parMapM (fewestPresses sum joltages) input
  print (sum res2)

fewestPresses :: SymVal a => ([SBV Double] -> SBV a) -> (Machine -> [a]) -> Machine -> IO Integer
fewestPresses how what m@(MkM _ buttons _) = getRes <$> optimize Lexicographic (do
  -- create SMT variables for each button
  vs <- mapM mkVar [0 .. length buttons - 1]
  -- assert constraints for each state/joltage
  zipWithM_ (addConstraint vs) [0..] (what m)
  -- minimize the sum of all button variables (total presses)
  minimize "total_presses" (sum $ map snd vs))
  where
    addConstraint presses idx a =
      constrain (literal a .== how [ v | (btnIdx, v) <- presses, idx `elem` (buttons !! btnIdx) ])
    mkVar idx = do
      v <- symbolic (show idx)
      constrain (v .>= 0)
      return (idx, v)
    getRes (LexicographicResult model)
      | Just n <- getModelValue "total_presses" model
             = n
    getRes _ = error "Optimization failed"

parseMachine :: Parsec String () Machine
parseMachine = MkM <$>
  (char '[' *> many1 state <* char ']') <*>
  (spaces *> buttons `sepEndBy1` spaces) <*>
  (char '{' *> (int `sepBy1` char ',') <* char '}' <* newline)
  where
    state =  (False <$ char '.')
         <|> (True  <$ char '#' )
    buttons = char '(' *>
      int `sepBy1` char ',' <*
      char ')'
    int :: Read a => Parsec String () a
    int = read <$> many1 digit
