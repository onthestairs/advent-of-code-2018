module Day07(
    solve1',
    solve2',
) where

import Relude

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (ord)
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe dependenciesListParser
solve2' = solve2 <<$>> parseMaybe dependenciesListParser

data Dependency a = Dependency {
  from :: a,
  to :: a
}
type DependencyGraph a = Map.Map a (Set.Set a)
type Step = Char
dependencyParser :: Parser (Dependency Step)
dependencyParser = do
  _ <- string "Step "
  n <- upperChar
  _ <- string " must be finished before step "
  v <- upperChar
  _ <- string " can begin."
  pure $ Dependency n v

dependenciesListParser :: Parser [Dependency Step]
dependenciesListParser = sepBy dependencyParser newline

makeDependencyGraph :: Ord a => [Dependency a] -> DependencyGraph a
makeDependencyGraph ds = foldl' (\dg (Dependency from to) -> Map.insertWith Set.union to (Set.singleton from) dg) initialGraph ds
  where allKeys = concatMap (\(Dependency from to) -> [from, to]) ds
        initialGraph = Map.fromList $ map (\k -> (k, Set.empty)) allKeys

findFrontier :: Ord a => DependencyGraph a -> Maybe (NonEmpty a)
findFrontier = nonEmpty . sort . Map.keys . Map.filter (\v -> Set.size v == 0)

removeNode :: Ord a => a -> DependencyGraph a -> DependencyGraph a
removeNode x dg = (Map.delete x dg)

removeDependencies :: Ord a => a -> DependencyGraph a -> DependencyGraph a
removeDependencies x = Map.map (\v -> Set.delete x v)

deleteElement :: Ord a => a -> DependencyGraph a -> DependencyGraph a
deleteElement x dg = removeDependencies x (removeNode x dg)

deleteElements :: (Ord a, Foldable f) => f a -> DependencyGraph a -> DependencyGraph a
deleteElements xs dg = foldl' (flip deleteElement) dg xs

traverseInOrder :: Ord a => DependencyGraph a -> [a]
traverseInOrder dg' = reverse $ go [] dg'
  where go :: Ord a => [a] -> DependencyGraph a -> [a]
        go xs dg = case findFrontier dg of
                      Just (f NE.:| fs) -> go (f:xs) (deleteElement f dg)
                      Nothing -> xs

solve1 :: [Dependency Char] -> [Char]
solve1 ds = traverseInOrder $ makeDependencyGraph ds

--------

type Seconds = Int
data ElfState = Idle | WorkingOnStep Step Seconds deriving (Eq, Show)
stepTime step = ((ord step) - 64) + 60

foldAndGather :: Foldable f => ((a, b) -> (a, b)) -> b -> f a -> ([a], b)
foldAndGather f start xs = foldl' (\(as, b) a -> let (a', b') = f (a, b) in (a':as, b') ) ([], start) xs

elfTick :: (ElfState, DependencyGraph Step) -> (ElfState, DependencyGraph Step)
elfTick (Idle, dg) = (Idle, dg)
elfTick (WorkingOnStep s 1, dg) = (Idle, removeDependencies s dg)
elfTick (WorkingOnStep s n, dg) = (WorkingOnStep s (n-1), dg)

tick :: ([ElfState], DependencyGraph Step) -> ([ElfState], DependencyGraph Step)
tick (es, dg) = foldAndGather elfTick dg es

tryToFindWork :: DependencyGraph Step -> (ElfState, DependencyGraph Step)
tryToFindWork dg = case findFrontier dg of
  Just (f NE.:| _) -> (WorkingOnStep f (stepTime f), removeNode f dg)
  Nothing -> (Idle, dg)

elfPickUpWork :: (ElfState, DependencyGraph Step) -> (ElfState, DependencyGraph Step)
elfPickUpWork (Idle, dg) = tryToFindWork dg
elfPickUpWork s = s

pickUpWork :: ([ElfState], DependencyGraph Step) -> ([ElfState], DependencyGraph Step)
pickUpWork (es, dg) = foldAndGather elfPickUpWork dg es

doElvesTimeStep'  :: ([ElfState], DependencyGraph Step) -> ([ElfState], DependencyGraph Step)
doElvesTimeStep' =  pickUpWork . tick

putElvesToWork :: Int -> DependencyGraph Step -> [[ElfState]]
putElvesToWork n dg = map fst $ takeWhile (any ((/=) Idle) . fst) $ iterate doElvesTimeStep' $ pickUpWork (replicate n Idle, dg)

solve2 :: [Dependency Step] -> Int
solve2 ds = length $ putElvesToWork 5 (makeDependencyGraph ds)