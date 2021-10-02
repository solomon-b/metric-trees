{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
module Data.BKTree (Metric (..), BKTree, Lev (..), insert, query) where

import qualified Data.Map.Strict.Internal as M
import Data.Map.Internal (link)
import Data.Monoid

between :: (Ord k) => k -> k -> M.Map k a -> M.Map k a
between _ _ M.Tip = M.Tip
between lo hi (M.Bin _ kx x l r)
    | kx < lo = between lo hi r
    | kx > hi = between lo hi l
    | otherwise = link kx x (between lo hi l) (between lo hi r)

-- | Metric Spaces.
-- Laws:
-- * @dist x y = 0@ if and only if @x == y@
-- * @dist x y = dist y x@
-- * @dist x z <= dist x y <> dist y z@
class (Monoid v, Ord v) => Metric v a | a -> v where
    dist :: a -> a -> v

data BKTree v a = Node !a !(M.Map v (BKTree v a))
  deriving Show

singleton :: a -> BKTree v a
singleton a = Node a M.empty

insert :: (Metric v a) => a -> BKTree v a -> BKTree v a
insert a (Node root edges) =
    let d = dist a root
    in if | d == mempty                    -> Node root edges
          | Just t <- (M.lookup d edges) -> insert a t
          | otherwise                      -> Node root (M.insert d (singleton a) edges)

query :: Num v => Enum v => Metric v a => a -> v -> BKTree v a -> [(a, v)]
query new d_max (Node root edges) =
  let d_root = dist root new
      candidates = between (d_root - d_max) (d_root + d_max) edges
      rs = foldMap (query new d_max) candidates
  in if d_root < d_max then (root, d_root):rs else rs

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
