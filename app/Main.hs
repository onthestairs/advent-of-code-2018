{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Relude

import qualified Text.Megaparsec as Megaparsec
import qualified Data.Map
import Options.Applicative
import System.Console.Pretty (Color (..), Style (..), color, style)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09

printColouredEither :: (Show a, Show b) => Either a b -> IO ()
printColouredEither (Left a) = putTextLn (color Red (show a) :: Text)
printColouredEither (Right b) = putTextLn (color Green (show b) :: Text)

printColouredMaybe :: (Show a) => Maybe a -> IO ()
printColouredMaybe Nothing  = putTextLn (color Red "Did not solve" :: Text)
printColouredMaybe (Just a) = putTextLn (color Green (show a) :: Text)

type Day = Int
data Solver = forall a. (Show a) => Solver {
  inputFilename :: String,
  solve :: Text -> Maybe a
}
data DaySolvers f = DaySolvers (f Solver) (f Solver)

solvers :: Map Day (DaySolvers Maybe)
solvers = fromList [
    (1, DaySolvers (Just $ Solver "./inputs/01/1.txt" Day01.solve1') (Just $ Solver "./inputs/01/1.txt" Day01.solve2')),
    (2, DaySolvers (Just $ Solver "./inputs/02/1.txt" Day02.solve1') (Just $ Solver "./inputs/02/1.txt" Day02.solve2')),
    (3, DaySolvers (Just $ Solver "./inputs/03/1.txt" Day03.solve1') (Just $ Solver "./inputs/03/1.txt" Day03.solve2')),
    (4, DaySolvers (Just $ Solver "./inputs/04/1.txt" Day04.solve1') (Just $ Solver "./inputs/04/1.txt" Day04.solve2')),
    (5, DaySolvers (Just $ Solver "./inputs/05/1.txt" Day05.solve1') (Just $ Solver "./inputs/05/1.txt" Day05.solve2')),
    (6, DaySolvers (Just $ Solver "./inputs/06/1.txt" Day06.solve1') (Just $ Solver "./inputs/06/1.txt" Day06.solve2')),
    (7, DaySolvers (Just $ Solver "./inputs/07/1.txt" Day07.solve1') (Just $ Solver "./inputs/07/1.txt" Day07.solve2')),
    (8, DaySolvers (Just $ Solver "./inputs/08/1.txt" Day08.solve1') (Just $ Solver "./inputs/08/1.txt" Day08.solve2')),
    (9, DaySolvers (Just $ Solver "./inputs/09/1.txt" Day09.solve1') (Just $ Solver "./inputs/09/1.txt" Day09.solve2')),
    (10, DaySolvers Nothing Nothing),
    (11, DaySolvers Nothing Nothing),
    (12, DaySolvers Nothing Nothing),
    (13, DaySolvers Nothing Nothing),
    (14, DaySolvers Nothing Nothing),
    (15, DaySolvers Nothing Nothing),
    (16, DaySolvers Nothing Nothing),
    (17, DaySolvers Nothing Nothing),
    (18, DaySolvers Nothing Nothing),
    (19, DaySolvers Nothing Nothing),
    (20, DaySolvers Nothing Nothing),
    (21, DaySolvers Nothing Nothing),
    (22, DaySolvers Nothing Nothing),
    (23, DaySolvers Nothing Nothing),
    (24, DaySolvers Nothing Nothing),
    (25, DaySolvers Nothing Nothing)]

runSolver filename solver = do
  fileContents <- readFileText filename
  print (solver fileContents)

solveKnownDayPart :: Int -> Maybe Solver -> IO ()
solveKnownDayPart part maybeSolver = do
  putText (System.Console.Pretty.style Italic "Part " <> show part <> ": " :: Text)
  case maybeSolver of
    Just (Solver filename solver) -> do
      fileContents1 <- readFileText filename
      let solution1 = solver fileContents1
      printColouredMaybe solution1
    Nothing -> putTextLn (color Red "Not implemented" :: Text)

solveKnownDay :: Day -> DaySolvers Maybe -> IO ()
solveKnownDay day (DaySolvers solver1 solver2) = do
  putTextLn (System.Console.Pretty.style Underline ("Day " <> show day) :: Text)
  solveKnownDayPart 1 solver1
  solveKnownDayPart 2 solver2

solveAllDays = forM_ (Data.Map.assocs solvers) (uncurry solveKnownDay)

solveDay :: Day -> IO ()
solveDay day = case Data.Map.lookup day solvers of
    Nothing -> putTextLn ("Couldn't find solvers for day " <> show day :: Text)
    (Just solvers) -> solveKnownDay day solvers

data SolveOptions = SolveDay Day | SolveAll
solveDayOptions :: Parser SolveOptions
solveDayOptions = SolveDay <$> option auto
  (  long "day"
  <> short 'd'
  <> help "Run solver for a single day"
  <> metavar "INT" )

solveAllOptions :: Parser SolveOptions
solveAllOptions = flag' SolveAll
  (  long "all"
  <> short 'a'
  <> help "Run all the solvers" )

solveOptions :: Parser SolveOptions
solveOptions = solveDayOptions <|> solveAllOptions

opts = info (solveOptions <**> helper)
      ( fullDesc
     <> progDesc "Solve advent of code 2018"
     <> header "Advent of code 2018" )

main :: IO ()
main = do
  options <- execParser opts
  case options of
    (SolveDay day) -> solveDay day
    SolveAll -> solveAllDays
