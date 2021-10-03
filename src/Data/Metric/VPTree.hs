{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Metric.VPTree where

import Control.Monad.ST
import Data.Metric.Class
import qualified Data.Vector as V
import qualified System.Random.MWC.Probability as P
import GHC.Generics

data VPTree v a = Empty | Node
  { _mu :: !v
  -- ^ Median distance to vantage point
  , _vp :: !a
  -- ^ Vantage point
  , _near :: !(VPTree v a)
  -- ^ Points at a distance < mu
  , _far :: !(VPTree v a)
  -- ^ Points a distance > mu
  } deriving (Show, Eq, Foldable, Generic)

withST :: (forall s. P.Gen s -> ST s a) -> a
withST f = runST $ P.create >>= f

-- | Construct a 'VPTree' from vector of points.
--
-- 1. Pick a vantage point
-- 2. Measure the distance of all other points to the VP
-- 3. Partition the points around the median
-- 4. Recurse
build :: (Ord v, Fractional v, Metric v a) => V.Vector a -> VPTree v a
build xs = withST $ \gen -> do
  i <- P.sample @_ @Int (P.uniformR (0, length xs)) gen
  case fmap V.uncons (V.splitAt i xs) of
    (_, Nothing) -> error $ "The impossible has occured: Index '" <> show i <> "' out of bounds."
    (points1, Just (vp, points2)) ->
      -- TODO: Pick a random index for the vantage point
      let points = points1 <> points2
          dists = fmap (\pt -> (dist vp pt, pt)) points
          mu = median (fmap fst dists)
          (lefts, rights) = V.partition (\(v, _) -> v < mu) dists
      in pure $ Node mu vp (build (fmap snd lefts)) (build (fmap snd rights))

-- | Find points in the tree closer to 'a' then the threshold distance 'v'.
find :: Metric v a => VPTree v a -> v -> a -> [(v, a)]
find Empty _ _ = []
find (Node _ vp closer further) v a =
  let d = dist vp a
  in if d < v
       then (d, vp) : find closer v a
       else find further v a

median :: Ord a => V.Vector a -> a
median xs = select (V.length xs `div` 2) xs

select :: Ord a => Int -> V.Vector a -> a
select i xs
  | V.length xs <= 5 = xs V.! i
  | lengthLower == i = medianOfMedians
  | lengthLower < i = select (i - lengthLower - 1) upperPartition
  | otherwise = select i lowerPartition
  where
    medianOfMedians = median (fmap median (chunksOf 5 xs))
    (lowerPartition, upperPartition) = V.partition (< medianOfMedians) xs
    lengthLower = length lowerPartition

chunksOf :: Int -> V.Vector a -> V.Vector (V.Vector a)
chunksOf n xs =
  if V.null xs
    then mempty
    else
      let (beginning, rest) = V.splitAt n xs
      in V.cons beginning (chunksOf n rest)
