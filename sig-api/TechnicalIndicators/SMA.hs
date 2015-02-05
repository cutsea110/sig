module TechnicalIndicators.SMA (sma) where

import Control.Applicative ((<$>), (<*>))
import Data.List (tails, unfoldr)
import Data.Maybe (isNothing, fromJust)

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

-- | Global Proposition : We suggest that the list of key value pair has been sorted by key.
--   This module just only calculate simple moving value.

sma :: Fractional v => Int -> [(k, Maybe v)] -> [(k, v)]
sma n = uncurry zip . cross (drop (n-1), map (average . take n) . tails) . unzip . cleansing

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs

cleansing :: [(k, Maybe v)] -> [(k, v)]
cleansing = uncurry zip . cross (id, repair) . unzip . dropWhile (isNothing . snd)

repair :: [Maybe a] -> [a]
repair xxs@(Just x:xs) = unfoldr f (x, xxs)
    where
      f (old, []) = Nothing
      f (old, x:xs) = Just $ maybe nothing just x
        where
          nothing = (old, (old, xs))
          just new = (new, (new, xs))
repair [] = []
repair (Nothing:xs) = error "'repair' expect Just value as head of argument list."
