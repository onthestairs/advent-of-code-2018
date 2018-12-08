{-# LANGUAGE DeriveFunctor, DeriveFoldable, TypeFamilies #-}

module Day08(
    solve1',
    solve2',
) where

import Relude

import Data.Functor.Foldable
import Data.List ((!!))
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators

import Utils.Parsing


-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe treeParser
solve2' = solve2 <<$>> parseMaybe treeParser

type MetaData = [Int]
data Tree a = Tree a [Tree a] deriving (Show, Functor, Foldable)
-- recursion schemes instances
data TreeF a r = TreeF a [r] deriving (Show, Eq, Functor)
type instance Base (Tree a) = TreeF a
instance Recursive (Tree a) where
  project (Tree x cs) = TreeF x cs
instance Corecursive (Tree a) where
  embed (TreeF x cs) = Tree x cs

nSepBy :: Int -> Parser a -> Parser b -> Parser [b]
nSepBy 0 sep p = pure $ []
nSepBy 1 sep p = do
  a <- p
  pure $ [a]
nSepBy n sep p = do
  a <- p
  _ <- sep
  rest <- nSepBy (n-1) sep p
  pure $ (a:rest)

metaDataParser :: Int -> Parser MetaData
metaDataParser n = nSepBy n (char ' ') intParser

treeParser :: Parser (Tree MetaData)
treeParser = do
    numberOfChildren <- intParser
    _ <- char ' '
    numberOfMetaDataEntries <- intParser
    _ <- optional (char ' ')
    children <- nSepBy numberOfChildren (char ' ') treeParser
    _ <- optional (char ' ')
    metaData <- metaDataParser numberOfMetaDataEntries
    pure $ Tree metaData children

solve1 :: Tree MetaData -> Int
solve1 = cata algebra where
  algebra (TreeF metadata children) = sum metadata + sum children

safeLookup xs n = if (n < length xs) && (n >= 0) then Just (xs !! n) else Nothing
lookupWithDefault d xs n = fromMaybe d (safeLookup xs n)

solve2 :: Tree MetaData -> Int
solve2 = cata algebra
  where algebra (TreeF metadata []) = sum metadata
        algebra (TreeF metadata xs) = sum $ map (lookupWithDefault 0 xs . pred) metadata