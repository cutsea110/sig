module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Applicative (Applicative, (<$>), (<*>), pure)

import TechnicalIndicators.Core

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
