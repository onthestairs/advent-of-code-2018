module Day12(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.Parsing
import Utils.List

-- end to end solving functions
solve1' = solve1 <=< parseMaybe inputParser
solve2' = solve2 <=< parseMaybe inputParser

data Bit = Off | On deriving (Eq, Ord, Show)
type Transitions =  Map (Bit, Bit, Bit, Bit, Bit) Bit
data Input = Input {
  _initialState :: Set.Set Int,
  _transitions :: Transitions
} deriving (Show)

bitParser :: Parser Bit
bitParser = (char '#' $> On) <|> (char '.' $> Off)

initialStateParser :: Parser (Set.Set Int)
initialStateParser = do
  bits <- Text.Megaparsec.some bitParser
  let onBits = map fst $ filter ((==) On . snd) $ zip [0..] bits
  pure $ Set.fromList onBits

transitionParser :: Parser ((Bit, Bit, Bit, Bit, Bit), Bit)
transitionParser = do
  b1 <- bitParser
  b2 <- bitParser
  b3 <- bitParser
  b4 <- bitParser
  b5 <- bitParser
  _ <- string " => "
  output <- bitParser
  pure $ ((b1, b2, b3, b4, b5), output)

inputParser :: Parser Input
inputParser = do
  _ <- string "initial state: "
  initialState <- initialStateParser
  _ <- newline
  _ <- newline
  transitions <- sepBy transitionParser newline
  pure $ Input initialState (Map.fromList transitions)

getPossibleRange :: Set.Set Int -> [Int]
getPossibleRange xs = [minOnX - 4 .. maxOnX + 4]
  where
    nonEmptyXs = nonEmpty (Set.toList xs)
    minOnX = fromMaybe 0 $ minimum1 <$> nonEmptyXs
    maxOnX = fromMaybe 0 $ maximum1 <$> nonEmptyXs

getWindow :: Set.Set Int -> Int -> (Bit, Bit, Bit, Bit, Bit)
getWindow bits n = (lookup (n-2), lookup (n-1), lookup n, lookup (n+1), lookup (n+2))
  where lookup m = if Set.member m bits then On else Off

step :: Transitions -> Set.Set Int -> Set.Set Int
step transitions bits = Set.fromList $ map fst $ filter ((==) On . snd) $ zipWithF (lookup . window) range
  where range = getPossibleRange bits
        window n = getWindow bits n
        lookup window@(_, _, b, _, _) = Map.findWithDefault Off window transitions

onPositionsSum bits = sum $ Set.toList bits

solve1 (Input initialState transitions) = fmap onPositionsSum $ viaNonEmpty head $ drop 20 $ iterate (step transitions) initialState

diffs :: Num a => [a] -> Maybe [a]
diffs xs = do
  rest <- nonEmpty xs
  pure $ zipWith (-) (tail rest) xs

triples :: [a] -> Maybe [(a, a, a)]
triples xs = do
  ys <- (viaNonEmpty tail) xs
  zs <- (viaNonEmpty tail) ys
  pure $ zip3 xs ys zs

all3Equal :: Eq a => (a, a, a) -> Bool
all3Equal (x, y, z) = (x == y) && (y == z)

data ConvergencePoint = ConvergencePoint {
  _previousValues :: [Int],
  _startValue :: Int,
  _delta :: Int
} deriving (Show)

findValueInFuture :: ConvergencePoint -> Int -> Int
findValueInFuture (ConvergencePoint previousValues startValue delta) n =
  case nonEmpty (drop (n-1) previousValues) of
    Just (x :| xs) -> x
    Nothing -> (startValue + (delta * (futureToTake - 1)))
               where futureToTake = max 0 (n - (length previousValues))

findConvergencePoint :: [Int] -> Maybe ConvergencePoint
findConvergencePoint xs = do
  ds <- diffs xs
  ts <- triples ds
  let convergenceN = length $ takeWhile (not . all3Equal) ts
  startValue <- viaNonEmpty head $ drop convergenceN xs
  delta <- viaNonEmpty head $ drop convergenceN ds
  pure $ ConvergencePoint (take convergenceN xs) startValue delta

solve2 (Input initialState transitions) = do
  let sums = map onPositionsSum $ drop 1 $ iterate (step transitions) initialState
  covergencePoint <- findConvergencePoint sums
  pure $ findValueInFuture covergencePoint 50000000000