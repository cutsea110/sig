module TechnicalIndicators.SMA (sma) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow
import Data.List (tails)

sma ::Fractional v => Int -> [(k, v)] -> [(k, v)]
sma n = uncurry zip . (drop (n-1) *** map (average . take n) .tails) . unzip

average :: Fractional a => [a] -> a
average xs = s / l
    where
      (s, l) = foldr (\a (x, y) -> (a + x, 1 + y)) (0,0) xs
