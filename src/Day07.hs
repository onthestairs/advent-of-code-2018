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

removeManyDependencies :: (Ord a, Foldable f) => f a -> DependencyGraph a -> DependencyGraph a
removeManyDependencies  xs dg = foldl' (flip removeDependencies) dg xs

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

type Seconds = Int
data ElfState = Idle | WorkingOnStep Step Seconds deriving (Eq, Show)
stepTime step = ((ord step) - 64) + 60

startWorkingOnStep :: Step -> DependencyGraph Step -> ElfState
startWorkingOnStep s dg = WorkingOnStep s (stepTime s)

tryToFindWork :: DependencyGraph Step -> (ElfState, DependencyGraph Step)
tryToFindWork dg = case findFrontier dg of
  Just (f NE.:| _) -> (WorkingOnStep f (stepTime f), removeNode f dg)
  Nothing -> (Idle, dg)

doElfTimeStep :: (ElfState, DependencyGraph Step) -> (ElfState, DependencyGraph Step)
doElfTimeStep (WorkingOnStep s 1, dg) = tryToFindWork dg
doElfTimeStep (WorkingOnStep s n, dg) = (WorkingOnStep s (n-1), dg)
doElfTimeStep (Idle, dg) = tryToFindWork dg

elfTimeStepAccumulator :: ([ElfState], DependencyGraph Step) -> ElfState -> ([ElfState], DependencyGraph Step)
elfTimeStepAccumulator (es, dg) e = (e':es, dg')
  where (e', dg') = doElfTimeStep (e, dg)

releaseUpcomingSteps :: DependencyGraph Step -> [ElfState] -> DependencyGraph Step
releaseUpcomingSteps dg es = removeManyDependencies toRelease dg
  where toRelease = mapMaybe shouldRelease es
        shouldRelease (WorkingOnStep s 1) = Just s
        shouldRelease _ = Nothing

doElvesTimeStep :: ([ElfState], DependencyGraph Step) -> ([ElfState], DependencyGraph Step)
doElvesTimeStep (es, dg) = foldl' elfTimeStepAccumulator ([], dg') es
  where dg' = releaseUpcomingSteps dg es

putElvesToWork :: Int -> DependencyGraph Step -> [[ElfState]]
putElvesToWork n dg = map fst $ takeWhile (any ((/=) Idle) . fst) $ drop 1 $ iterate doElvesTimeStep (replicate n Idle, dg)

solve2 :: [Dependency Step] -> Int
solve2 ds = length $ putElvesToWork 5 (makeDependencyGraph ds)