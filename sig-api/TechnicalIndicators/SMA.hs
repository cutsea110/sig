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

average :: Fractional v => Int -> [v] -> v
average n xs = scanl (+) 0 xs !! n / fromIntegral n

average2 :: Fractional v => (Int, Int) -> [v] -> (v, v)
average2 (a, b) xs = (ttl !! a / fromIntegral a, ttl !! b / fromIntegral b)
    where
      ttl = scanl (+) 0 xs

para :: Fractional v => (Int, Int) -> ([k], [[Maybe v]]) -> ([(k, Maybe v)], [(k, Maybe v)])
para (a, b) (ks, vss) = (zip ka va, zip kb vb)
    where
      (va, vb) = unzip $ map (average2 (a, b)) vss
      (ka, kb) = let ks' = tails ks in (ks' !! (a-1) , ks' !! (b-1))

average3 :: Fractional v => (Int, Int, Int) -> [v] -> (v, v, v)
average3 (a, b, c) xs = (ttl !! a / fromIntegral a, ttl !! b / fromIntegral b, ttl !! c / fromIntegral c)
    where
      ttl = scanl (+) 0 xs

para3 :: Fractional v => (Int, Int, Int) -> ([k], [[Maybe v]])
       -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
para3 (a, b, c) (ks, vss) = (zip ka va, zip kb vb, zip kc vc)
    where
      (va, vb, vc) = unzip3 $ map (average3 (a, b, c)) vss
      (ka, kb, kc) = let ks' = tails ks in (ks' !! (a-1), ks' !! (b-1), ks' !! (c-1))

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
