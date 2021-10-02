{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Metric.Class where

import Data.Monoid (Sum (..))

-- | Metric Spaces.
-- Laws:
-- * @dist x y = 0@ if and only if @x == y@
-- * @dist x y = dist y x@
-- * @dist x z <= dist x y <> dist y z@
class (Monoid v, Ord v) => Metric v a | a -> v where
    dist :: a -> a -> v

newtype Lev = Lev String
  deriving Show

instance Metric (Sum Int) Lev where
  dist (Lev x) (Lev y) = lev x y

lev :: String -> String -> Sum Int
lev a b
  | null b = Sum $ length a
  | null a = Sum $ length b
  | head a == head b = lev (tail a) (tail b)
  | otherwise = 1 <> minimum [lev (tail a) b, lev a (tail b), lev (tail a) (tail b)]

-- TODO: A more efficient method would never repeat the same distance
-- calculation. For example, the Levenshtein distance of all possible
-- prefixes might be stored in an array M M where M [ i ] [ j ] is the
-- distance between the last i i characters of string s and the last j
-- j characters of string t. The table is easy to construct one row at
-- a time starting with row 0. When the entire table has been built,
-- the desired distance is in the table in the last row and column,
-- representing the distance between all of the characters in s and
-- all the characters in t.
