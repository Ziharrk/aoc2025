{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Redundant bracket" -}
module Day3 where

import Data.Proxy (Proxy(..))
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits (type (+), type (*), type (-), CmpNat, Nat, KnownNat, natVal)

import Language.Haskell.TH (Type(..), TyLit(..), runIO)

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
    xs' = tailAfterFirst m xs

tailAfterFirst :: Eq a => a -> [a] -> [a]
tailAfterFirst _ [] = []
tailAfterFirst y (z:zs)
  | y == z    = zs
  | otherwise = tailAfterFirst y zs

-- part1ViaTypes :: Integer
-- part1ViaTypes = natVal' (Proxy @(Sum (MapJolts 2 Input)))

natVal' :: forall n. KnownNat n => Integer
natVal' = natVal (Proxy @n)

type Input = $(runIO $ do
  input <- map (map (read . return)) . lines <$> readFile "input/day3"
  let go1 [] = ConT '[]
      go1 (x:xs) = AppT (AppT PromotedConsT (go2 x)) (go1 xs)
      go2 [] = ConT '[]
      go2 (y:ys) = AppT (AppT PromotedConsT (LitT (NumTyLit y))) (go2 ys)
  return (go1 input))

type family Sum (n :: [Nat]) :: Nat where
  Sum '[] = 0
  Sum (x ': xs) = x + Sum xs

type family MapJolts (n :: Nat) (xs :: [[Nat]]) :: [Nat] where
  MapJolts n '[]       = '[]
  MapJolts n (x ': xs) = ToNat (MaxJoltsWith n x) ': MapJolts n xs

type family ToNat (n :: [Nat]) :: Nat where
  ToNat xs = ToNat' 0 xs

type family ToNat' (acc :: Nat) (xs :: [Nat]) :: Nat where
  ToNat' acc '[]       = acc
  ToNat' acc (x ': xs) = ToNat' ((10 * acc) + x) xs

type family MaxJoltsWith (k :: Nat) (xs :: [Nat]) :: [Nat] where
  MaxJoltsWith 0 xs = '[]
  MaxJoltsWith k xs = Maximum (Take (Length xs - (k - 1)) xs)
    ': MaxJoltsWith (k - 1) (TailAfterFirst (Maximum (Take (Length xs - (k - 1)) xs)) xs)

type family Length (xs :: [a]) :: Nat where
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs

type family Take (n :: Nat) (xs :: [a]) :: [a] where
  Take 0 xs       = '[]
  Take n '[]      = '[]
  Take n (x ': xs) = x ': Take (n - 1) xs

type family TailAfterFirst (y :: a) (xs :: [a]) :: [a] where
  TailAfterFirst y '[] = '[]
  TailAfterFirst y (y ': xs) = xs
  TailAfterFirst y (x ': xs) = TailAfterFirst y xs

type family Maximum (xs :: [Nat]) :: Nat where
  Maximum '[x]           = x
  Maximum (x ': y ': xs) = MaxNat x (Maximum (y ': xs))

type family MaxNat (x :: Nat) (y :: Nat) :: Nat where
  MaxNat x y = If (CmpNat x y == 'GT) x y
