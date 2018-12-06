module Utils.Map where

import Relude

import qualified Data.Ord
import qualified Data.Map as Map

insertManyWith :: (Ord k, Foldable f) => (a -> a -> a) -> f k -> a -> Map.Map k a -> Map.Map k a
insertManyWith f ks v m = foldl' (\m' k -> Map.insertWith f k v m') m ks

maximumByValue :: (Ord k, Ord v) => Map.Map k v -> Maybe (k, v)
maximumByValue = (viaNonEmpty head) . sortOn (Data.Ord.Down . snd) . Map.toList

minimumByValue :: (Ord k, Ord v) => Map.Map k v -> Maybe (k, v)
minimumByValue = (viaNonEmpty head) . sortOn snd . Map.toList

counter :: Ord a => [a] -> Map a Int
counter = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty