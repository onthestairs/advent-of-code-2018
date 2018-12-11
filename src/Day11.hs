module Day11(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

-- import Control.Lens
import Linear.V2
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Ord

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <=< parseMaybe serialNumberParser
solve2' = solve2 <=< parseMaybe serialNumberParser

type Point = V2 Int
type SerialNumber = Int
serialNumberParser :: Parser SerialNumber
serialNumberParser = intParser

hundredsDigit :: Int -> Int
hundredsDigit n = justHundreds `div` 100
  where
    justHundreds = n `mod` 1000

cellPowerLevel :: SerialNumber -> Point -> Int
cellPowerLevel serialNumber (V2 x y) = powerLevel4
  where
    rackId = x + 10
    powerLevel0 = rackId * y
    powerLevel1 = powerLevel0 + serialNumber
    powerLevel2 = powerLevel1 * rackId
    powerLevel3 = hundredsDigit powerLevel2
    powerLevel4 = powerLevel3 - 5

data Grid = Grid {
  topLeft :: Point,
  bottomRight :: Point,
  points :: Map.Map Point Int
} deriving (Show)
type Size = Int

data SquareValue = SquareValue {
    _topLeft :: Point,
    _size :: Size,
    _value :: Int
} deriving (Show)
instance Eq SquareValue where
  (==) (SquareValue _ _ value) (SquareValue _ _ value') = (==) value value'
instance Ord SquareValue where
  compare (SquareValue _ _ value) (SquareValue _ _ value') = compare value value'

makeGrid :: SerialNumber -> Point -> Point -> Grid
makeGrid serialNumber tl@(V2 minX minY) br@(V2 maxX maxY) = Grid tl br $ Map.fromList $ map (\p -> (p, (cellPowerLevel serialNumber p))) points
  where points = [V2 x y | x <- [minX..maxX], y <- [minY..maxY]]

type RectangleMap = Point -> Int
makeRectangleValuesCache :: Grid -> RectangleMap
makeRectangleValuesCache (Grid (V2 minX minY) (V2 maxX maxY) points) = lookup
  where
    xs = [minX..maxX]
    ys = [minY..maxY]
    colSlitherMaps = Map.fromSet makeColSlitherMap (Set.fromList xs)
    makeColSlitherMap x = Map.fromList $ zip ys $ drop 1 $ scanl (colMapAccum x) 0 ys
    colMapAccum x a y = a + (Map.findWithDefault 0 (V2 x y) points)
    colSlitherLookup m y = Map.findWithDefault 0 y m
    lookup (V2 x y) = sum $ map (\x' -> colSlitherLookup (Map.findWithDefault Map.empty x' colSlitherMaps) y) [1..x]

findSquareValue :: RectangleMap -> Point -> Size -> Int
findSquareValue r (V2 left top) size = (r (V2 right bottom)) - (r (V2 right (top - 1))) - (r (V2 (left - 1) bottom)) +  (r (V2 (left - 1) (top - 1)))
  where
    bottom = top + size - 1
    right = left + size - 1

allSquareValues :: Grid -> [Size] -> [SquareValue]
allSquareValues grid@(Grid (V2 minX minY) (V2 maxX maxY) points) sizes = map (\(p, size) -> SquareValue p size (findSquareValue rectangleMap p size)) squares
  where squares = [(V2 x y, size) | x <- [1..maxX], y <- [1..maxY], size <- sizes, x + size <= maxX + 1, y + size <= maxY + 1]
        rectangleMap = makeRectangleValuesCache grid

showCoords :: SquareValue -> String
showCoords (SquareValue (V2 left top) _ _) = show left <> "," <> show top

solve1 serialNumber = showCoords <$> viaNonEmpty maximum1 (allSquareValues grid [3])
 where
  grid = makeGrid serialNumber (V2 1 1) (V2 300 300)

showCoordsAndSize :: SquareValue -> String
showCoordsAndSize (SquareValue (V2 left top) size _) = show left <> "," <> show top <> "," <> show size

solve2 :: SerialNumber -> Maybe String
solve2 serialNumber = showCoordsAndSize <$> viaNonEmpty maximum1 (allSquareValues grid [1..300])
 where
  grid = makeGrid serialNumber (V2 1 1) (V2 300 300)

testGrid = Grid (V2 1 1) (V2 4 4) (Map.fromList $ map (\(x, y) -> (V2 x y, 1)) [(x, y) | x <- [1..4], y <- [1..4]])