module Day03(
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
solve1' = solve1 <<$>> parseMaybe claimListParser
solve2' = solve2 <<$>> parseMaybe claimListParser

data Claim = Claim {
  claimId :: Int,
  leftPad :: Int,
  topPad :: Int,
  width :: Int,
  height :: Int
} deriving (Show)

claimParser :: Parser Claim
claimParser = do
  _ <- char '#'
  id <- intParser
  _ <- char ' '
  _ <- "@"
  _ <- char ' '
  leftPad <- intParser
  _ <- char ','
  topPad <- intParser
  _ <- char ':'
  _ <- char ' '
  width <- intParser
  _ <- char 'x'
  height <- intParser
  pure $ Claim id leftPad topPad width height


claimListParser :: Parser [Claim]
claimListParser = sepBy claimParser newline

type Coords = (Int, Int)

claimRange :: Claim -> [Coords]
claimRange (Claim id leftPad topPad width height) =
  [(x, y) | x <- [topPad..topPad+height-1], y <- [leftPad..leftPad+width-1]]

makeDensityMap :: [Claim] -> Map.Map Coords Int
makeDensityMap cs = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty (concatMap claimRange cs)

solve1 :: [Claim] -> Int
solve1 = length . Map.filter (> 1) . makeDensityMap

claimIsNotOverlapped :: Map.Map Coords Int -> Claim -> Bool
claimIsNotOverlapped overlaps claim = all ok (claimRange claim)
  where
    ok claim = case Map.lookup claim overlaps of
      Nothing -> False
      (Just n) -> n == 1

solve2 :: [Claim] -> [Int]
solve2 cs = map claimId $ filter (claimIsNotOverlapped (makeDensityMap cs)) cs