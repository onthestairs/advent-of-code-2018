module Utils.Map where

import Relude

import qualified Data.Map as Map

insertManyWith :: (Ord k, Foldable f) => (a -> a -> a) -> f k -> a -> Map.Map k a -> Map.Map k a
insertManyWith f ks v m = foldl' (\m' k -> Map.insertWith f k v m') m ks

maximumByValue :: (Ord k, Ord v) => Map.Map k v -> Maybe (k, v)
maximumByValue = (viaNonEmpty head) . reverse . sortOn snd . Map.toList