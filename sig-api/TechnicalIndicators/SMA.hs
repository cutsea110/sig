module TechnicalIndicators.SMA (sma) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.List (tails, unfoldr)
import Data.Maybe (isNothing, fromJust)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, v)]
sma n = uncurry zip . (drop (n-1) *** map (average . take n) . tails) . unzip . cleansing

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs

cleansing :: [(k, Maybe v)] -> [(k, v)]
cleansing = uncurry zip . (id *** repair) . unzip . dropWhile (isNothing . snd)

-- | This function expect the argument list has Just value as head element.
repair :: [Maybe a] -> [a]
repair xxs@(Just x:xs) = unfoldr step (x, xxs)
    where
      step (old, []) = Nothing
      step (old, x:xs) = let just = (,) <*> (,xs) in Just $ maybe (just old) just x
repair [] = []
repair (Nothing:xs) = error "'repair' expect Just value as head of argument list."
