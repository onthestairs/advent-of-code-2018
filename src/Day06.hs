module Day06(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Linear.V2

import Utils.Parsing
import Utils.Map

-- end to end solving functions
solve1' = solve1 <=< parseMaybe coordsListParser
solve2' = solve2 <=< parseMaybe coordsListParser

type Point = V2 Int
coordsParser :: Parser Point
coordsParser = do
    x <- intParser
    _ <- string ", "
    y <- intParser
    pure $ V2 x y

coordsListParser :: Parser [Point]
coordsListParser = sepBy coordsParser newline

manhattanDistance :: Point -> Point -> Int
manhattanDistance c1 c2 = sum $ abs (c1 - c2)

data BoundingBox = BoundingBox {
    topLeft :: Point,
    bottomRight :: Point
}
makeBoundingBoxCoords :: BoundingBox -> [Point]
makeBoundingBoxCoords (BoundingBox (V2 x1 y1) (V2 x2 y2)) =
    [V2 x y | x <- [x1..x2], y <- [y1..y2]]

x (V2 x' _) = x'
y (V2 _ y') = y'
makeBoundingBox :: NonEmpty Point -> BoundingBox
makeBoundingBox coords = BoundingBox (V2 left top) (V2 right bottom)
    where left   = minimum1 $ NE.map x coords
          right  = maximum1 $ NE.map x coords
          top    = minimum1 $ NE.map y coords
          bottom = maximum1 $ NE.map y coords

edgePoints :: BoundingBox -> Set.Set Point
edgePoints (BoundingBox (V2 x1 y1) (V2 x2 y2)) =
  Set.fromList $ top <> left <> bottom <> right
  where top    = [V2 x y1 | x <- [x1..x2]]
        left   = [V2 x1 y | y <- [y1..y2]]
        bottom = [V2 x y2 | x <- [x1..x2]]
        right  = [V2 x2 y | y <- [y1..y2]]

data NearestNeighbour = Single Point | Multiple deriving (Eq, Ord, Show)
findNearestNeighbour :: Point -> NonEmpty Point -> NearestNeighbour
findNearestNeighbour c cs =
  case NE.sortWith (manhattanDistance c) cs of
    x NE.:| [] -> Single x
    x NE.:| (y:ys) -> if (manhattanDistance c x) == (manhattanDistance c y) then Multiple else Single x

makeNearestNeighbourMap :: NonEmpty Point -> BoundingBox -> Map.Map Point NearestNeighbour
makeNearestNeighbourMap cs boundingBox = Map.fromList $ map (\c -> (c, findNearestNeighbour c cs)) boxCoords
  where boxCoords = makeBoundingBoxCoords boundingBox

findPointsWhichProjectToEdge :: BoundingBox -> Map.Map Point NearestNeighbour -> Set Point
findPointsWhichProjectToEdge boundingBox m = Set.fromList $ catMaybes $ Set.toList $ Set.map lookupPoint edges
  where edges = edgePoints boundingBox
        lookupPoint edgePoint = case Map.lookup edgePoint m of
          Nothing -> Nothing
          Just (Single nearPoint) -> Just nearPoint
          Just Multiple -> Nothing

nearestNeighbourToPoint Multiple = Nothing
nearestNeighbourToPoint (Single p) = Just p

solve1 :: [Point] -> Maybe Int
solve1 ps = do
  nonEmptyPoints <- nonEmpty ps
  let boundingBox = makeBoundingBox nonEmptyPoints
  let nearestNeighboursMap = makeNearestNeighbourMap nonEmptyPoints boundingBox
  let pointsWhichProjectToEdge = findPointsWhichProjectToEdge boundingBox nearestNeighboursMap
  let counts = counter $ mapMaybe nearestNeighbourToPoint (Map.elems nearestNeighboursMap)
  let innerPointsOnly = Map.filterWithKey (\p _ -> not (Set.member p pointsWhichProjectToEdge)) counts
  snd <$> maximumByValue innerPointsOnly

cumulativeDistance :: NonEmpty Point -> Point -> Int
cumulativeDistance ps p = sum $ NE.toList $ NE.map (manhattanDistance p) ps

solve2 :: [Point] -> Maybe Int
solve2 ps = do
  nonEmptyPoints <- nonEmpty ps
  let boundingBox = makeBoundingBox nonEmptyPoints
  let locations = makeBoundingBoxCoords boundingBox
  pure $ length $ filter ((>) 10000 . cumulativeDistance nonEmptyPoints) locations