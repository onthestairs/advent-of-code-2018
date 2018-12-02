module Day02(
    solve1',
    solve2',
) where

import Relude

import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Arrow

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <<$>> parseFile "./inputs/02/1.txt" charListParser
solve2' = solve2 <<$>> parseFile "./inputs/02/1.txt" charListParser

charListParser :: Parser [[Char]]
charListParser = sepBy (Text.Megaparsec.some letterChar) (string "\n")

hasNelems :: Ord a => Int -> [a] -> Bool
hasNelems n xs = n `elem` (Map.elems count)
  where count = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty xs

solve1 :: (Ord a) => [[a]] -> Int
solve1 xs = (length $ filter (hasNelems 2) xs) * (length $ filter (hasNelems 3) xs)

isApproximatelyEqual :: (Eq a) => [a] -> [a] -> Bool
isApproximatelyEqual xs ys = length (filter (uncurry (/=)) (zip xs ys)) == 1

solve2 :: Ord a => [[a]] -> [[a]]
solve2 xs = map extractAnswer $ filter (uncurry isApproximatelyEqual) pairs
  where pairs = [(cs, ds) | cs <- xs, ds <- xs, cs < ds]
        extractAnswer (xs, ys) = map fst $ filter (uncurry (==)) (zip xs ys)
