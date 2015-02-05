module TechnicalIndicators.SMA (sma) where

import Control.Applicative ((<$>), (<*>))
import Data.List (tails, unfoldr)

cross (f, g) (x, y) = (f x, g y)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, v)] -> [(k, v)]
sma n = uncurry zip . cross (drop (n-1), map (average . take n) . tails) . unzip

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs

sanitize :: [Maybe a] -> [Maybe a]
sanitize xs = unfoldr f (Nothing, xs)
  where
    f (old, []) = Nothing
    f (old, (Nothing:xs)) = Just (old, (old, xs))
    f (old, (Just x:xs)) = Just (Just x, (Just x, xs))
