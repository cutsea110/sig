module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Arrow ((***))
import Control.Applicative (Applicative, (<$>), (<*>), liftA, liftA2, pure)
import Data.List (tails, unfoldr, unzip4, unzip5)

-- | special DSL which only use in this module
--   The argument `x` is supplied to only function `g`.
(~>) :: (a -> b) -> (t -> b -> c) -> t -> a -> c
f ~> g = (.) <$> g <*> const f

dup a = (a, a)
dup3 a = (a, a, a)
dup4 a = (a, a, a, a)
dup5 a = (a, a, a, a, a)

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

cleansing :: [Maybe a] -> [Maybe a]
cleansing xs = unfoldr f (Nothing, xs)
  where
    f (old, []) = Nothing
    f (old, x:xs) = Just $ maybe nothing just x
        where
          nothing = (old, (old, xs))
          just x' = let new = Just x' in (new, (new, xs))

divBy :: Fractional a => [a] -> Int -> a
divBy ttl n = ttl !! n / fromIntegral n

averageN :: (Applicative f, Num v, Fractional (f v)) =>
            ((Int -> f v) -> tuples -> tuples') -> tuples -> [f v] -> tuples'
averageN _pair ps xs = scanl (+) (pure 0) xs `_divBy` ps
    where
      _divBy = _pair . divBy

wDivBy :: Fractional a => [(a, a, t)] -> Int -> a
wDivBy xs c = let (n, d, _) = xs !! c in n / d

waverageN :: (Applicative f, Num v, Fractional (f v)) =>
             ((Int -> f v) -> tuples -> tuples') -> tuples -> [f v] -> tuples'
waverageN _pair ps xs = scanl plus (dup3 (pure 0)) xs `_divBy` ps
    where
      plus (ttl, wttl, w) x = let w' = w+1 in (ttl+x*w', wttl+w', w')
      _divBy = _pair . wDivBy

{-
let xs = map Just [530,540,535,530,520,510,500,499,510,490,550,440,590,600,610,580,615,580,895,600,600,605,620,575,595,600]
-}
eaverageN :: (Applicative f, Num a, Fractional (f a)) =>
     ((Int -> (f a, f a)) -> tuples -> tuples') -> tuples -> [f a] -> tuples'
eaverageN _pair ps xs = _pair (_divBy <*> accum) ps
    where
      _divBy n = cross ((/ fromIntegral n), id)
      accum n = scanl plus (dup (pure 0)) xs !! n
          where
            plus (ttl, prev) x = (ttl+x, prev+(x-prev)*alpha)
            alpha = 2 / fromIntegral (n-1)

from :: [a] -> Int -> [a]
from xs n = tails xs !! (n-1)

single :: ((a2 -> a2) -> Int -> a1 -> b)
       -> Int -> ([a], [a1]) -> [(a, b)]
single average n = uncurry zip . (flip from n *** map (average id n))

-- | general utility function for para#
paraN _pair _zip _unzip _average ps (ks, vss)
    = (ks `_from` ps) `_zip` (_unzip $ map (_average ps) vss)
      where
        _from = _pair . from

para :: (((a2 -> b1) -> (a2, a2) -> (b1, b1))
             -> (Int, Int) -> a1 -> (b, b))
     -> (Int, Int) -> ([a], [a1]) -> ([(a, b)], [(a, b)])
para average = paraN pair (cross . pair zip) unzip (average pair)

para3 :: (((a2 -> b1) -> (a2, a2, a2) -> (b1, b1, b1))
              -> (Int, Int, Int) -> a1 -> (b, b, b))
      -> (Int, Int, Int) -> ([a], [a1]) -> ([(a, b)], [(a, b)], [(a, b)])
para3 average = paraN pair3 (cross3 . pair3 zip) unzip3 (average pair3)

para4 :: (((a2 -> b1) -> (a2, a2, a2, a2) -> (b1, b1, b1, b1))
              -> (Int, Int, Int, Int) -> a1 -> (b, b, b, b))
      -> (Int, Int, Int, Int)
      -> ([a], [a1])
      -> ([(a, b)], [(a, b)], [(a, b)], [(a, b)])
para4 average = paraN pair4 (cross4 . pair4 zip) unzip4 (average pair4)

para5 :: (((a2 -> b1) -> (a2, a2, a2, a2, a2) -> (b1, b1, b1, b1, b1))
              -> (Int, Int, Int, Int, Int) -> a1 -> (b, b, b, b, b))
      -> (Int, Int, Int, Int, Int)
      -> ([a], [a1])
      -> ([(a, b)], [(a, b)], [(a, b)], [(a, b)], [(a, b)])
para5 average = paraN pair5 (cross5 . pair5 zip) unzip5 (average pair5)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
sma = prepare ~> single averageN

sma2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
sma2 = prepare ~> para averageN

sma3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma3 = prepare ~> para3 averageN

sma4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma4 = prepare ~> para4 averageN

sma5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma5 = prepare ~> para5 averageN

instance Num v => Num (Maybe v) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional v => Fractional (Maybe v) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational
