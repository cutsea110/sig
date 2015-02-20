module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>), liftA, liftA2)
import Data.List (tails, unfoldr, unzip4, unzip5)

-- | special DSL which only use in this module
--   The argument `x` is supplied to only function `g`.
(~>) :: (a -> b) -> (t -> b -> c) -> t -> a -> c
f ~> g = (.) <$> g <*> const f

prepare :: [(k, Maybe v)] -> ([k], [[Maybe v]])
prepare = (id *** tails . cleansing) . unzip

single :: Fractional v => Int -> ([k], [[Maybe v]]) -> [(k, Maybe v)]
single n = uncurry zip . (from *** map (average n))
    where
      from xs = tails xs !! (n-1)

tuply :: (a -> b) -> (a, a) -> (b, b)
tuply f (a, b) = (f a, f b)

tuply3 :: (a -> b) -> (a, a, a) -> (b, b, b)
tuply3 f (a, b, c) = (f a, f b, f c)

tuply4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
tuply4 f (a, b, c, d) = (f a, f b, f c, f d)

tuply5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
tuply5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

divBy :: Fractional a => [a] -> Int -> a
divBy ttl n = ttl !! n / fromIntegral n

divBy2 :: Fractional a => [a] -> (Int, Int) -> (a, a)
divBy2 = tuply . divBy

divBy3 :: Fractional a => [a] -> (Int, Int, Int) -> (a, a, a)
divBy3 = tuply3 . divBy

divBy4 :: Fractional a => [a] -> (Int, Int, Int, Int) -> (a, a, a, a)
divBy4 = tuply4 . divBy

divBy5 :: Fractional a => [a] -> (Int, Int, Int, Int, Int) -> (a, a, a, a, a)
divBy5 = tuply5 . divBy

average :: Fractional v => Int -> [v] -> v
average n xs = scanl (+) 0 xs `divBy` n

average2 :: Fractional v => (Int, Int) -> [v] -> (v, v)
average2 (a, b) xs = scanl (+) 0 xs `divBy2` (a, b)

average3 :: Fractional v => (Int, Int, Int) -> [v] -> (v, v, v)
average3 (a, b, c) xs = scanl (+) 0 xs `divBy3` (a, b, c)

average4 :: Fractional v => (Int, Int, Int, Int) -> [v] -> (v, v, v, v)
average4 (a, b, c, d) xs = scanl (+) 0 xs `divBy4` (a, b, c, d)

average5 :: Fractional v => (Int, Int, Int, Int, Int) -> [v] -> (v, v, v, v, v)
average5 (a, b, c, d, e) xs = scanl (+) 0 xs `divBy5` (a, b, c, d, e)

from :: [a] -> Int -> [a]
from xs n = tails xs !! (n-1)

from2 :: [a] -> (Int, Int) -> ([a], [a])
from2 = tuply . from

from3 :: [a] -> (Int, Int, Int) -> ([a], [a], [a])
from3 = tuply3 . from

from4 :: [a] -> (Int, Int, Int, Int) -> ([a], [a], [a], [a])
from4 = tuply4 . from

from5 :: [a] -> (Int, Int, Int, Int, Int) -> ([a], [a], [a], [a], [a])
from5 = tuply5 . from

para :: Fractional v => (Int, Int) -> ([k], [[Maybe v]]) -> ([(k, Maybe v)], [(k, Maybe v)])
para ps (ks, vss) = (zip ka va, zip kb vb)
    where
      (va, vb) = unzip $ map (average2 ps) vss
      (ka, kb) = ks `from2` ps

para3 :: Fractional v => (Int, Int, Int) -> ([k], [[Maybe v]])
       -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para3 ps (ks, vss) = (zip ka va, zip kb vb, zip kc vc)
    where
      (va, vb, vc) = unzip3 $ map (average3 ps) vss
      (ka, kb, kc) = ks `from3` ps

para4 :: Fractional v => (Int, Int, Int, Int) -> ([k], [[Maybe v]])
      -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para4 ps (ks, vss) = (zip ka va, zip kb vb, zip kc vc, zip kd vd)
    where
      (va, vb, vc, vd) = unzip4 $ map (average4 ps) vss
      (ka, kb, kc, kd) = ks `from4` ps

para5 :: Fractional v => (Int, Int, Int, Int, Int) -> ([k], [[Maybe v]])
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para5 ps (ks, vss) = (zip ka va, zip kb vb, zip kc vc, zip kd vd, zip ke ve)
    where
      (va, vb, vc, vd, ve) = unzip5 $ map (average5 ps) vss
      (ka, kb, kc, kd, ke) = ks `from5` ps

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
