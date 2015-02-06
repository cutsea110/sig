module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

import Control.Applicative ((<$>), (<*>), liftA, liftA2)
import Data.List (tails, unfoldr)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

pair3 (f, g, h) x = (f x, g x, h x)
pair4 (f, g, h, k) x = (f x, g x, h x, k x)
pair5 (f, g, h, k, l) x = (f x, g x, h x, k x, l x)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
sma n = single n . prepare

sma2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
sma2 (n, m) = pair (single n, single m) . prepare

sma3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma3 (x, y, z) = pair3 (single x, single y, single z) . prepare

sma4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma4 (x, y, z, w) = pair4 (single x, single y, single z, single w) . prepare

sma5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma5 (x, y, z, w, v) = pair5 (single x, single y, single z, single w, single v) . prepare

prepare :: [(k, Maybe v)] -> ([k], [[Maybe v]])
prepare = cross (id, tails . cleansing) . unzip

single :: Fractional v => Int -> ([k], [[Maybe v]]) -> [(k, Maybe v)]
single n = uncurry zip . cross (drop (n-1), map (average . take n))

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs

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
