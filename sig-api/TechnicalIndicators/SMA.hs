module TechnicalIndicators.SMA (sma, sma2, sma3, sma4, sma5) where

-- | Simple Moving Average

import Control.Applicative (Applicative, pure)

import TechnicalIndicators.Core (single, para, para3, para4, para5)

divBy :: Fractional a => [a] -> Int -> a
divBy ttl n = ttl !! n / fromIntegral n

averageN :: (Applicative f, Num v, Fractional (f v)) =>
            ((Int -> f v) -> tuples -> tuples') -> tuples -> [f v] -> tuples'
averageN _pair ps xs = scanl (+) (pure 0) xs `_divBy` ps
    where
      _divBy = _pair . divBy

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
sma = single averageN

sma2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
sma2 = para averageN

sma3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma3 = para3 averageN

sma4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma4 = para4 averageN

sma5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
sma5 = para5 averageN
