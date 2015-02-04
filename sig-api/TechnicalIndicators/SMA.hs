module TechnicalIndicators.SMA (sma) where

import Control.Applicative ((<$>), (<*>))
import Data.List (tails)

cross (f, g) (x, y) = (f x, g y)

-- | proposition : We suggest that the list of key value pair has been sorted by key.
sma :: Fractional v => Int -> [(k, v)] -> [(k, v)]
sma n = uncurry zip . cross (drop (n-1), map (average . take n) . tails) . unzip

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs
