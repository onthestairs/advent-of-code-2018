{-# LANGUAGE TemplateHaskell #-}

module Day14(
    solve1',
    solve2',
) where

import Relude

import Control.Lens
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Sequence as Sequence
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.Parsing

data Elf = Elf {
  _position :: Int
} deriving (Show)
makeLenses ''Elf
type RecipeScores = Sequence.Seq Int
data RecipesState = RecipesState {
  _recipeScores :: RecipeScores,
  _elves :: [Elf]
} deriving (Show)
makeLenses ''RecipesState

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe intParser
solve2' = solve2 <=< parseMaybe intParser

elfRecipeScore :: RecipeScores -> Elf -> Int
elfRecipeScore scores (Elf position) = Sequence.index scores position

intDigits :: Int -> [Int]
intDigits n = reverse $ go n
  where go m = if m < 10 then [m] else (m `mod` 10):(go (m `div` 10))

makeNextRecipes :: RecipeScores -> [Elf] -> [Int]
makeNextRecipes scores elves = intDigits currentScore
  where
    currentScore = sum (map (elfRecipeScore scores) elves)

appendValues :: RecipeScores -> [Int] -> RecipeScores
appendValues scores ns = scores <> (Sequence.fromList ns)

moveElf :: RecipeScores -> Elf -> Elf
moveElf scores elf@(Elf position) = Elf $ (position + (elfRecipeScore scores elf) + 1) `mod` (Sequence.length scores)

tick :: RecipesState -> ([Int], RecipesState)
tick (RecipesState scores elves) = (newRecipeScores, RecipesState nextScores nextElves)
  where
    newRecipeScores = makeNextRecipes scores elves
    nextScores = appendValues scores newRecipeScores
    nextElves = map (moveElf nextScores) elves

showConcatSequence xs = foldl' (\a x -> a <> show x) "" xs

makeRecipeList :: [Int] -> RecipesState -> [Int]
makeRecipeList start state = start <> go state
  where
    go state' = let (next, nextState) = tick state' in next <> go nextState

solve1 :: Int -> Text
solve1 numberOfRecipes = showConcatSequence $ take 10 $ drop numberOfRecipes $ makeRecipeList [3,7] initialRecipesState
  where
    initialRecipesState = RecipesState (Sequence.fromList [3, 7]) [Elf 0, Elf 1]

prefixAt :: Ord a => [a] -> [a] -> Maybe Int
prefixAt ys xs = go 0 xs
  where
    yLength = length ys
    go n zs@(z:rest) = if take yLength zs == ys then Just n else go (n+1) rest
    go n [] = Nothing

solve2 recipeScoresToFind = prefixAt (intDigits recipeScoresToFind) $ makeRecipeList [3,7] initialRecipesState
  where
    initialRecipesState = RecipesState (Sequence.fromList [3, 7]) [Elf 0, Elf 1]