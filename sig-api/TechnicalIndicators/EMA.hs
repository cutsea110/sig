module TechnicalIndicators.EMA {- (ema, ema2, ema3, ema4, ema5) -} where

-- | Simple Moving Average

import Control.Applicative (Applicative, (<*>), pure)
import Control.Arrow (second)

import TechnicalIndicators.Core (pair, pair3, pair4, pair5, cross, dup, single, para, para3, para4, para5)

averageN :: (Applicative f, Num a, Fractional (f a)) =>
     ((Int -> (f a, f a)) -> tuples -> tuples') -> tuples -> [f a] -> tuples'
averageN _pair ps xs = _pair (_divBy <*> accum) ps
    where
      _divBy n = cross ((/ fromIntegral n), id)
      accum n = scanl plus (dup (pure 0)) xs !! n
          where
            plus (ttl, prev) x = (ttl+x, prev+(x-prev)*alpha)
            alpha = 2 / fromIntegral (n-1)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

collector :: [(a, (b, b))] -> [(a, b)]
collector = zipWith second $ fst:repeat snd

ema :: Fractional v => Int -> [(k, Maybe v)] -> [(k, Maybe v)]
ema n = collector . single averageN n

ema2 :: Fractional v =>
     (Int, Int) -> [(k, Maybe v)] -> ([(k, Maybe v)], [(k, Maybe v)])
ema2 ps = pair collector . para averageN ps

ema3 :: Fractional v =>
     (Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
ema3 ps = pair3 collector . para3 averageN ps

ema4 :: Fractional v =>
     (Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
ema4 ps = pair4 collector . para4 averageN ps

ema5 :: Fractional v =>
     (Int, Int, Int, Int, Int)
     -> [(k, Maybe v)]
     -> ([(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)], [(k, Maybe v)])
ema5 ps = pair5 collector . para5 averageN ps
