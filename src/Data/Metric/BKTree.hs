{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Data.Metric.BKTree (Metric (..), BKTree, Lev (..), insert, query) where

import qualified Data.Map.Strict.Internal as M
import Data.Map.Internal (link)
import Data.Metric.Class

between :: (Ord k) => k -> k -> M.Map k a -> M.Map k a
between _ _ M.Tip = M.Tip
between lo hi (M.Bin _ kx x l r)
    | kx < lo = between lo hi r
    | kx > hi = between lo hi l
    | otherwise = link kx x (between lo hi l) (between lo hi r)

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
