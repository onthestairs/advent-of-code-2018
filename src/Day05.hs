module Day05(
    solve1',
    solve2',
) where

import Relude

import Data.Char
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.Parsing
import Utils.Map

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe polymerParser
solve2' = solve2 <=< parseMaybe polymerParser

type Polymer = [Char]
polymerParser :: Parser Polymer
polymerParser = Text.Megaparsec.many letterChar

triggersReaction :: Char -> Char -> Bool
triggersReaction x y = x /= y && toUpper x == toUpper y

react :: [Char] -> [Char]
react [] = []
react [x] = [x]
react (x:y:xs) = if triggersReaction x y then xs else x:react(y:xs)

converge f x = let x' = f x in if x == x' then x else converge f x'
fullyReactSlow = converge react

fullyReact :: Polymer -> Polymer
fullyReact p = reverse $ go [] p
  where
    go [] (c:cs) = go [c] cs
    go xs [] = xs
    go (x:xs) (c:cs) = if triggersReaction x c then go xs cs else go (c:x:xs) cs

afterAllReactionsLength = length . fullyReact

solve1 :: Polymer -> Int
solve1 = afterAllReactionsLength

type Unit = Char
removeUnit :: Unit -> Polymer -> Polymer
removeUnit u = filter (\c -> (c /= u && c /= (toUpper u)))

allUnits = ['a'..'z']

solve2 p = fmap snd $ minimumByValue $ Map.fromList $ map (\u -> (u, afterAllReactionsLength (removeUnit u p))) $ allUnits
