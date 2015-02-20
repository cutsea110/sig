module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>), liftA, liftA2)
import Data.List (tails, unfoldr)

-- | special DSL which only use in this module
--   The argument `x` is supplied to only function `g`.
(~>) :: (a -> b) -> (t -> b -> c) -> t -> a -> c
f ~> g = (.) <$> g <*> const f

prepare :: [(k, Maybe v)] -> ([k], [[Maybe v]])
prepare = (id *** tails . cleansing) . unzip

single :: Fractional v => Int -> ([k], [[Maybe v]]) -> [(k, Maybe v)]
single n = uncurry zip . (drop (n-1) *** map (average n))

average :: Fractional v => Int -> [Maybe v] -> Maybe v
average n xs = scanl (+) 0 xs !! n / fromIntegral n

para :: Fractional v => (Int, Int) -> ([k], [[Maybe v]]) -> ([(k, Maybe v)], [(k, Maybe v)])
para (a, b) x = (single a x, single b x)

para3 :: Fractional v => (Int, Int, Int) -> ([k], [[Maybe v]])
      -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para3 (a, b, c) x = (single a x, single b x, single c x)

para4 :: Fractional v => (Int, Int, Int, Int) -> ([k], [[Maybe v]])
      -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para4 (a, b, c, d) x = (single a x, single b x, single c x, single d x)

para5 :: Fractional v => (Int, Int, Int, Int, Int) -> ([k], [[Maybe v]])
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para5 (a, b, c, d, e) x = (single a x, single b x, single c x, single d x, single e x)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
sma = prepare ~> single

sma2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
sma2 = prepare ~> para

sma3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma3 = prepare ~> para3

sma4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma4 = prepare ~> para4

sma5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma5 = prepare ~> para5

cleansing :: [Maybe a] -> [Maybe a]
cleansing xs = unfoldr f (Nothing, xs)
  where
    f (old, []) = Nothing
    f (old, x:xs) = Just $ maybe nothing just x
        where
          nothing = (old, (old, xs))
          just x' = let new = Just x' in (new, (new, xs))

instance Num v => Num (Maybe v) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = Just . fromInteger

instance Fractional v => Fractional (Maybe v) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = Just . fromRational
