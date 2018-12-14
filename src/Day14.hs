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
solve1' = solve1 <=< parseMaybe intParser
solve2' = solve2 <=< parseMaybe intParser

elfRecipeScore :: RecipeScores -> Elf -> Int
-- elfRecipeScore scores (Elf position) = fromMaybe 0 (scores Vector.!? position)
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

tick :: RecipesState -> RecipesState
tick (RecipesState scores elves) = RecipesState nextScores nextElves
  where
    newRecipeScores = makeNextRecipes scores elves
    nextScores = appendValues scores newRecipeScores
    nextElves = map (moveElf nextScores) elves

-- showConcatSequence :: (Foldable f, Show a) => f a -> Text
showConcatSequence xs = foldl' (\a x -> a <> show x) "" xs

-- solve1 :: Int -> Int
solve1 numberOfRecipes = fmap extractAnswer $ find isLong $ iterate tick initialRecipesState
  where
    initialRecipesState = RecipesState (Sequence.fromList [3, 7]) [Elf 0, Elf 1]
    isLong = ((\s -> s >= numberOfRecipes + 10) . Sequence.length . view recipeScores)
    extractAnswer =  showConcatSequence . Sequence.drop numberOfRecipes . view recipeScores

prefixAt :: Ord a => [a] -> [a] -> Maybe Int
prefixAt ys xs = go 0 xs
  where
    yLength = length ys
    go n zs@(z:rest) = if take yLength zs == ys then Just n else go (n+1) rest
    go n [] = Nothing

solve2 recipeScoresToFind = fmap (prefixAt prefix) $ viaNonEmpty head $ map (toList . view recipeScores) $ drop 100000000 $ iterate tick initialRecipesState
  where
    prefix = intDigits recipeScoresToFind
    -- prefix = intDigits 51589
    -- prefixLength = length prefix
    -- extractAnswer = ((-) prefixLength) . Sequence.length . view recipeScores
    initialRecipesState = RecipesState (Sequence.fromList [3, 7]) [Elf 0, Elf 1]