{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Day08(
    solve1',
    solve2',
) where

import Relude

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
solve1 = sum . fmap sum

safeLookup n xs = if (n < length xs) && (n >= 0) then Just (xs !! n) else Nothing

getMetaDataValue :: [Tree MetaData] -> Int -> Int
getMetaDataValue xs n = case safeLookup (n-1) xs of
  Just child -> getNodeValue child
  Nothing -> 0

getNodeValue :: Tree MetaData -> Int
getNodeValue (Tree metadata []) = sum metadata
getNodeValue (Tree metadata xs) = sum $ map (getMetaDataValue xs) metadata

solve2 :: Tree MetaData -> Int
solve2 = getNodeValue
