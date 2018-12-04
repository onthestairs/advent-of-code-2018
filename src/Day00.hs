module Day00 (
    solve1',
    solve2',
) where

import Relude

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Arrow
import Data.Time.LocalTime
import Data.Time.Calendar

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <<$>> parseFile "./inputs/00/1.txt" morseCodeParser
solve2' = solve2 <<$>> parseFile "./inputs/00/2.txt" morseCodeParser

-- parsing
data MorseCodeItem = Dot | Dash deriving (Show, Eq)

morseCodeItemParser :: Parser MorseCodeItem
morseCodeItemParser = (string "." $> Dot) <|> (string "-" $> Dash)

morseCodeParser :: Parser [MorseCodeItem]
morseCodeParser = sepBy morseCodeItemParser (string ",")

solve1 :: [MorseCodeItem] -> Int
solve1 = length
solve2 :: [MorseCodeItem] -> Int
solve2 = (length . filter ((==) Dot))