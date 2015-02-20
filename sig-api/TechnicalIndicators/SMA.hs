module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>), liftA, liftA2)
import Data.List (tails, unfoldr, unzip4, unzip5)

-- | special DSL which only use in this module
--   The argument `x` is supplied to only function `g`.
(~>) :: (a -> b) -> (t -> b -> c) -> t -> a -> c
f ~> g = (.) <$> g <*> const f

pair :: (a -> b) -> (a, a) -> (b, b)
pair f (a, b) = (f a, f b)
pair3 :: (a -> b) -> (a, a, a) -> (b, b, b)
pair3 f (a, b, c) = (f a, f b, f c)
pair4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
pair4 f (a, b, c, d) = (f a, f b, f c, f d)
pair5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
pair5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

cross :: (a -> x, b -> y) -> (a, b) -> (x, y)
cross (f, g) (a, b) = (f a, g b)
cross3 :: (a -> x, b -> y, c -> z) -> (a, b, c) -> (x, y, z)
cross3 (f, g, h) (a, b, c) = (f a, g b, h c)
cross4 :: (a -> x, b -> y, c -> z, d -> w) -> (a, b, c, d) -> (x, y, z, w)
cross4 (f, g, h, j) (a, b, c, d) = (f a, g b, h c, j d)
cross5 :: (a -> x, b -> y, c -> z, d -> w, e -> v) -> (a, b, c, d, e) -> (x, y, z, w, v)
cross5 (f, g, h, j, k) (a, b, c, d, e) = (f a, g b, h c, j d, k e)

prepare :: [(k, Maybe v)] -> ([k], [[Maybe v]])
prepare = (id *** tails . cleansing) . unzip

single :: Fractional v => Int -> ([k], [[Maybe v]]) -> [(k, Maybe v)]
single n = uncurry zip . (from *** map (average n))
    where
      from xs = tails xs !! (n-1)

divBy :: Fractional a => [a] -> Int -> a
divBy ttl n = ttl !! n / fromIntegral n

divBy2 :: Fractional a => [a] -> (Int, Int) -> (a, a)
divBy2 = pair . divBy

divBy3 :: Fractional a => [a] -> (Int, Int, Int) -> (a, a, a)
divBy3 = pair3 . divBy

divBy4 :: Fractional a => [a] -> (Int, Int, Int, Int) -> (a, a, a, a)
divBy4 = pair4 . divBy

divBy5 :: Fractional a => [a] -> (Int, Int, Int, Int, Int) -> (a, a, a, a, a)
divBy5 = pair5 . divBy

average :: Fractional v => Int -> [v] -> v
average n xs = scanl (+) 0 xs `divBy` n

average2 :: Fractional v => (Int, Int) -> [v] -> (v, v)
average2 ps xs = scanl (+) 0 xs `divBy2` ps

average3 :: Fractional v => (Int, Int, Int) -> [v] -> (v, v, v)
average3 ps xs = scanl (+) 0 xs `divBy3` ps

average4 :: Fractional v => (Int, Int, Int, Int) -> [v] -> (v, v, v, v)
average4 ps xs = scanl (+) 0 xs `divBy4` ps

average5 :: Fractional v => (Int, Int, Int, Int, Int) -> [v] -> (v, v, v, v, v)
average5 ps xs = scanl (+) 0 xs `divBy5` ps

from :: [a] -> Int -> [a]
from xs n = tails xs !! (n-1)

from2 :: [a] -> (Int, Int) -> ([a], [a])
from2 = pair . from

from3 :: [a] -> (Int, Int, Int) -> ([a], [a], [a])
from3 = pair3 . from

from4 :: [a] -> (Int, Int, Int, Int) -> ([a], [a], [a], [a])
from4 = pair4 . from

from5 :: [a] -> (Int, Int, Int, Int, Int) -> ([a], [a], [a], [a], [a])
from5 = pair5 . from

para :: Fractional v => (Int, Int) -> ([k], [[Maybe v]]) -> ([(k, Maybe v)], [(k, Maybe v)])
para ps (ks, vss) = (cross . pair zip) (ka, kb) (va, vb)
    where
      (va, vb) = unzip $ map (average2 ps) vss
      (ka, kb) = ks `from2` ps

para3 :: Fractional v => (Int, Int, Int) -> ([k], [[Maybe v]])
       -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para3 ps (ks, vss) = (cross3 . pair3 zip) (ka, kb, kc) (va, vb, vc)
    where
      (va, vb, vc) = unzip3 $ map (average3 ps) vss
      (ka, kb, kc) = ks `from3` ps

para4 :: Fractional v => (Int, Int, Int, Int) -> ([k], [[Maybe v]])
      -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para4 ps (ks, vss) = (cross4 . pair4 zip) (ka, kb, kc, kd) (va, vb, vc, vd)
    where
      (va, vb, vc, vd) = unzip4 $ map (average4 ps) vss
      (ka, kb, kc, kd) = ks `from4` ps

para5 :: Fractional v => (Int, Int, Int, Int, Int) -> ([k], [[Maybe v]])
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para5 ps (ks, vss) = (cross5 . pair5 zip) (ka, kb, kc, kd, ke) (va, vb, vc, vd, ve)
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
