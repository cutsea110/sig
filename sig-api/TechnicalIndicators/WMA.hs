module TechnicalIndicators.WMA (wma, wma2, wma3, wma4, wma5) where

-- | Simple Moving Average

import Control.Applicative (Applicative, pure)

import TechnicalIndicators.Core (dup3, single, para, para3, para4, para5)

divBy :: Fractional a => [(a, a, t)] -> Int -> a
divBy xs c = let (n, d, _) = xs !! c in n / d

averageN :: (Applicative f, Num v, Fractional (f v)) =>
            ((Int -> f v) -> tuples -> tuples') -> tuples -> [f v] -> tuples'
averageN _pair ps xs = scanl plus (dup3 (pure 0)) xs `_divBy` ps
    where
      plus (ttl, wttl, w) x = let w' = w+1 in (ttl+x*w', wttl+w', w')
      _divBy = _pair . divBy

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

wma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
wma = single averageN

wma2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
wma2 = para averageN

wma3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
wma3 = para3 averageN

wma4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
wma4 = para4 averageN

wma5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
wma5 = para5 averageN
