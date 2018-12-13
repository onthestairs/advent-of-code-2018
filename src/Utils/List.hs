module Utils.List where

import Relude

zipWithF :: (a -> b) -> [a] -> [(a, b)]
zipWithF f xs = zip xs (map f xs)