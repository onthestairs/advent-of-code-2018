{-# LANGUAGE TemplateHaskell #-}

module Day13(
    solve1',
    solve2',
) where

import Relude hiding (Down)

import Control.Lens hiding (Empty)
import Linear.V2
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.Parsing
import Utils.List

type Point = V2 Int
data Track = RightSlant | LeftSlant | Vertical | Horizontal | Cross | Empty
  deriving (Eq, Show)
type Grid = Map.Map Point Track
data Direction = L | R | U | D deriving (Eq, Show)
data Cart = Cart {
  _currentPosition :: Point,
  _currentDirection :: Direction,
  _lastCrossroadsTurn :: Maybe Direction
} deriving (Eq, Show)
makeLenses ''Cart

directionDelta :: Direction -> V2 Int
directionDelta U = V2 0 (-1)
directionDelta D = V2 0 1
directionDelta L = V2 (-1) 0
directionDelta R = V2 1 0

-- end to end solving functions
solve1' = solve1 <=< parseMaybe gridParser
solve2' = solve2 <=< parseMaybe gridParser

trackParser :: Parser Track
trackParser = do
  (char '/' $> RightSlant) <|>  (char '\\' $> LeftSlant) <|>  (char '|' $> Vertical) <|>  (char '-' $> Horizontal) <|>  (char '+' $> Cross) <|>  (char ' ' $> Empty)

cartDirectionParser :: Parser Direction
cartDirectionParser = do
  (char '^' $> U) <|>  (char 'v' $> D) <|>  (char '<' $> L) <|>  (char '>' $> R)

cellParser :: Parser (Either Track Direction)
cellParser = do
  eitherP trackParser cartDirectionParser

-- gridParser :: Parser Grid
gridParser = do
  cs <- sepBy (Text.Megaparsec.many cellParser) newline
  pure $ cs

emptyGrid = Map.empty

addCellToGridAndCarts :: (Grid, [Cart]) -> Point -> Either Track Direction -> (Grid, [Cart])
addCellToGridAndCarts (g, cs) p (Left Empty) = (g, cs)
addCellToGridAndCarts (g, cs) p (Left trackPiece) = (Map.insert p trackPiece g, cs)
addCellToGridAndCarts (g, cs) p (Right cartPiece) = (Map.insert p (cartTotrackPiece cartPiece) g, c:cs)
  where
    c = Cart p cartPiece Nothing
    cartTotrackPiece U = Vertical
    cartTotrackPiece D = Vertical
    cartTotrackPiece L = Horizontal
    cartTotrackPiece R = Horizontal

cellsToGridAndCarts :: [[Either Track Direction]] -> (Grid, [Cart])
cellsToGridAndCarts css = foldl' foldRow (emptyGrid, []) (zip [0..] css)
  where foldRow (grid, carts) (row, cols) = foldl' (foldCol row) (grid, carts) (zip [0..] cols)
        foldCol row (grid, carts) (col, cell) = addCellToGridAndCarts (grid, carts) (V2 col row) cell

crossroadsDirection :: Direction -> Direction -> Direction
crossroadsDirection U L = L
crossroadsDirection U U = U
crossroadsDirection U R = R
crossroadsDirection L L = D
crossroadsDirection L U = L
crossroadsDirection L R = U
crossroadsDirection D L = R
crossroadsDirection D U = D
crossroadsDirection D R = L
crossroadsDirection R L = U
crossroadsDirection R U = R
crossroadsDirection R R = D

nextCrossroadsTurn :: Maybe Direction -> Direction
nextCrossroadsTurn Nothing = L
nextCrossroadsTurn (Just L) = U
nextCrossroadsTurn (Just U) = R
nextCrossroadsTurn (Just R) = L

tickCart :: Grid -> Cart -> Cart
tickCart g (Cart p d lastTurn) = Cart nextPoint (nextDirection d nextTrack) nextLastTurnDirection
  where
    nextPoint = p + (directionDelta d)
    nextTrack = Map.lookup nextPoint g
    nextTurnDirection = nextCrossroadsTurn lastTurn
    nextLastTurnDirection = case nextTrack of
      Just Cross -> Just nextTurnDirection
      _ -> lastTurn
    nextDirection U (Just Vertical) = U
    nextDirection U (Just RightSlant) = R
    nextDirection U (Just LeftSlant) = L
    nextDirection D (Just Vertical) = D
    nextDirection D (Just RightSlant) = L
    nextDirection D (Just LeftSlant) = R
    nextDirection R (Just Horizontal) = R
    nextDirection R (Just RightSlant) = U
    nextDirection R (Just LeftSlant) = D
    nextDirection L (Just Horizontal) = L
    nextDirection L (Just RightSlant) = D
    nextDirection L (Just LeftSlant) = U
    nextDirection d (Just Cross) = crossroadsDirection d nextTurnDirection

tickCarts :: Grid -> [Cart] -> [Cart]
tickCarts g cs = map (tickCart g) cs

combinations 0 _ = [[]]
combinations n ls = [ (x:ys) | (x:xs) <- tails ls, ys <- combinations (n-1) xs ]

findCollisionPoints :: [Cart] -> Set.Set Point
findCollisionPoints cs = Set.fromList $ mapMaybe (viaNonEmpty head) $ filter (\(x:y:zs) -> (x == y)) pairs
  where
    pairs = combinations 2 (map (view currentPosition) cs)

displayPoint :: V2 Int -> Text
displayPoint (V2 x y) = show x <> "," <> show y

solve1 cells = do
  let (g, carts) = cellsToGridAndCarts cells
  ps <- fmap Set.toList $ find ((==) 1 . Set.size) $ map findCollisionPoints $ iterate (tickCarts g) carts
  p <- viaNonEmpty head ps
  pure $ displayPoint p

deleteCollisions :: [Cart] -> [Cart]
deleteCollisions cs = filter (\c -> not $ Set.member (view currentPosition c) collisionPoints) cs
  where collisionPoints = findCollisionPoints cs

solve2 cells = do
  let (g, carts) = cellsToGridAndCarts cells
  cs <- find (\cs -> length cs == 1) $ iterate (deleteCollisions . tickCarts g) carts
  c <- viaNonEmpty head cs
  let p = view currentPosition c
  pure $ displayPoint p