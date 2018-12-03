module Day01(
    solve1',
    solve2',
) where

import Relude

import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Arrow

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe intListParser
solve2' = solve2 <<$>> parseMaybe intListParser

intListParser :: Parser [Int]
intListParser = sepBy signedIntParser (string "\n")

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: [Int] -> Int
solve2 xs =
  go Set.empty 0 frequencyChanges
  where
    frequencyChanges = cycle xs
    go seen currentFrequency (x:xs) =
      if Set.member nextFrequency seen
        then nextFrequency
        else go (Set.insert nextFrequency seen) nextFrequency xs
      where nextFrequency = currentFrequency + x
