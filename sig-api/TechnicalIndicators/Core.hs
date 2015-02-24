module TechnicalIndicators.Core where

import Control.Arrow ((***))
import Control.Applicative (Applicative, (<$>), (<*>), liftA, liftA2, pure)
import Data.List (tails, unfoldr, unzip4, unzip5)

-- | special DSL which only use in this module
--   The argument `x` is supplied to only function `g`.
(~>) :: (a -> b) -> (t -> b -> c) -> t -> a -> c
f ~> g = (.) <$> g <*> const f

dup a = (a, a)
dup3 a = (a, a, a)
dup4 a = (a, a, a, a)
dup5 a = (a, a, a, a, a)

pair :: (a -> b) -> (a, a) -> (b, b)
pair f (a, b) = (f a, f b)
pair3 :: (a -> b) -> (a, a, a) -> (b, b, b)
pair3 f (a, b, c) = (f a, f b, f c)
pair4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
pair4 f (a, b, c, d) = (f a, f b, f c, f d)
pair5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
pair5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

cross :: (a -> x, b -> y) -> (a, b) -> (x, y)
cross (f, g) (a, b) = (f a, g b)
cross3 :: (a -> x, b -> y, c -> z) -> (a, b, c) -> (x, y, z)
cross3 (f, g, h) (a, b, c) = (f a, g b, h c)
cross4 :: (a -> x, b -> y, c -> z, d -> w) -> (a, b, c, d) -> (x, y, z, w)
cross4 (f, g, h, j) (a, b, c, d) = (f a, g b, h c, j d)
cross5 :: (a -> x, b -> y, c -> z, d -> w, e -> v) -> (a, b, c, d, e) -> (x, y, z, w, v)
cross5 (f, g, h, j, k) (a, b, c, d, e) = (f a, g b, h c, j d, k e)

prepare :: [(k, Maybe v)] -> ([k], [[Maybe v]])
prepare = (id *** tails . cleansing) . unzip

cleansing :: [Maybe a] -> [Maybe a]
cleansing xs = unfoldr f (Nothing, xs)
  where
    f (old, []) = Nothing
    f (old, x:xs) = Just $ maybe nothing just x
        where
          nothing = (old, (old, xs))
          just x' = let new = Just x' in (new, (new, xs))

from :: [a] -> Int -> [a]
from xs n = tails xs !! (n-1)

single :: ((a2 -> a2) -> Int -> a1 -> b)
       -> Int -> ([a], [a1]) -> [(a, b)]
single average n = uncurry zip . (flip from n *** map (average id n))

-- | general utility function for para#
paraN _pair _zip _unzip _average ps (ks, vss)
    = (ks `_from` ps) `_zip` (_unzip $ map (_average ps) vss)
      where
        _from = _pair . from

para :: (((a2 -> b1) -> (a2, a2) -> (b1, b1))
             -> (Int, Int) -> a1 -> (b, b))
     -> (Int, Int) -> ([a], [a1]) -> ([(a, b)], [(a, b)])
para average = paraN pair (cross . pair zip) unzip (average pair)

para3 :: (((a2 -> b1) -> (a2, a2, a2) -> (b1, b1, b1))
              -> (Int, Int, Int) -> a1 -> (b, b, b))
      -> (Int, Int, Int) -> ([a], [a1]) -> ([(a, b)], [(a, b)], [(a, b)])
para3 average = paraN pair3 (cross3 . pair3 zip) unzip3 (average pair3)

para4 :: (((a2 -> b1) -> (a2, a2, a2, a2) -> (b1, b1, b1, b1))
              -> (Int, Int, Int, Int) -> a1 -> (b, b, b, b))
      -> (Int, Int, Int, Int)
      -> ([a], [a1])
      -> ([(a, b)], [(a, b)], [(a, b)], [(a, b)])
para4 average = paraN pair4 (cross4 . pair4 zip) unzip4 (average pair4)

para5 :: (((a2 -> b1) -> (a2, a2, a2, a2, a2) -> (b1, b1, b1, b1, b1))
              -> (Int, Int, Int, Int, Int) -> a1 -> (b, b, b, b, b))
      -> (Int, Int, Int, Int, Int)
      -> ([a], [a1])
      -> ([(a, b)], [(a, b)], [(a, b)], [(a, b)], [(a, b)])
para5 average = paraN pair5 (cross5 . pair5 zip) unzip5 (average pair5)

instance Num v => Num (Maybe v) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional v => Fractional (Maybe v) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational
