{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Metric.BKTree.Int where

import qualified Data.IntMap.Internal as M
import Data.Metric.Class
import Data.Monoid (Sum (..))

data BKTree a = Node !a !(M.IntMap (BKTree a))
  deriving Show

singleton :: a -> BKTree a
singleton a = Node a M.empty

insert :: (Metric (Sum Int) a) => a -> BKTree a -> BKTree a
insert a (Node root edges) =
    let (Sum d) = dist a root
    in if | d == 0                       -> Node root edges
          | Just t <- (M.lookup d edges) -> insert a t
          | otherwise                    -> Node root (M.insert d (singleton a) edges)

query :: Metric (Sum Int) a => a -> Int -> BKTree a -> [(a, Int)]
query new d_max (Node root edges) =
  let (Sum d_root) = dist root new
      candidates =
        M.filterWithKey (\k _ -> k < (d_root + d_max) && k > (d_root - d_max)) edges
      rs = foldMap (query new d_max) candidates
  in if d_root < d_max then (root, d_root):rs else rs
