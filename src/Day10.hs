{-# LANGUAGE TemplateHaskell #-}

module Day10(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

import Data.Text (pack, unpack)
import Control.Lens
import qualified Data.Set as Set
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Show (Show(..))
import Utils.Parsing

data Point = Point {
  _location :: V2 Int,
  _velocity :: V2 Int
} deriving (Show)
makeLenses ''Point

vectorParser :: Parser (V2 Int)
vectorParser = do
  _ <- char '<'
  _ <- optional (char ' ')
  x <- signedIntParser
  _ <- char ','
  _ <- char ' '
  _ <- optional (char ' ')
  y <- signedIntParser
  _ <- char '>'
  pure $ V2 x y

pointParser :: Parser Point
pointParser = do
  _ <- string "position="
  location <- vectorParser
  _ <- string " velocity="
  velocity <- vectorParser
  pure $ Point location velocity

pointsParser :: Parser [Point]
pointsParser = sepBy pointParser newline

data BoundingBox = BoundingBox {
  _topLeft :: V2 Int,
  _bottomRight :: V2 Int
} deriving (Show)
makeLenses ''BoundingBox

height :: BoundingBox -> Int
height (BoundingBox (V2 minX minY) (V2 maxX maxY)) = maxY - minY

getBoundingBoxRows :: BoundingBox -> [[V2 Int]]
getBoundingBoxRows (BoundingBox (V2 minX minY) (V2 maxX maxY)) = [[V2 x y | x <- [minX..maxX]] | y <- [minY..maxY]]

data PossibleMessage = PossibleMessage {
  _visiblePoints :: Set.Set (V2 Int),
  _boundingBox :: BoundingBox
}
makeLenses ''PossibleMessage

instance Show PossibleMessage where
  show message = "\n" <> (unpack $ unlines $ fmap (pack . fmap pointChar) rows)
    where rows = getBoundingBoxRows (view boundingBox message)
          pointChar p = if Set.member p (view visiblePoints message) then '#' else '.'

-- end to end solving functions
solve1' = solve1 <=< parseMaybe pointsParser
solve2' = solve2 <=< parseMaybe pointsParser

tick :: NonEmpty Point -> NonEmpty Point
tick ps = fmap (\p -> over location ((+) (view velocity p)) p) ps

makeBoundingBox :: NonEmpty Point -> BoundingBox
makeBoundingBox ps = BoundingBox (V2 minX minY) (V2 maxX maxY)
  where
    xs = fmap (view (location . _x)) ps
    minX = minimum1 xs
    maxX = maximum1 xs
    ys = fmap (view (location . _y)) ps
    minY = minimum1 ys
    maxY = maximum1 ys

toSet :: (Foldable f, Ord a) => f a -> Set.Set a
toSet xs = Set.fromList (toList xs)

draw :: NonEmpty Point -> PossibleMessage
draw ps = PossibleMessage (toSet (fmap (view location) ps)) (makeBoundingBox ps)

-- solve1 :: [Point] -> Int
solve1 ps = do
  nonEmptyPs <- nonEmpty ps
  final <- viaNonEmpty head $ dropWhile ((\h -> h > 10) . height . makeBoundingBox) $ iterate tick nonEmptyPs
  pure $ draw final

solve2 ps = do
  nonEmptyPs <- nonEmpty ps
  let final = length $ takeWhile ((\h -> h > 10) . height . makeBoundingBox) $ iterate tick nonEmptyPs
  pure $ final
