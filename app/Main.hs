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
import qualified Day10
import qualified Day11
import qualified Day12

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

data SolvedState a = Done a | DoneSlow a | NotDone

solvers :: Map Day (DaySolvers SolvedState)
solvers = fromList [
    (1, DaySolvers (Done $ Solver "./inputs/01/1.txt" Day01.solve1') (Done $ Solver "./inputs/01/1.txt" Day01.solve2')),
    (2, DaySolvers (Done $ Solver "./inputs/02/1.txt" Day02.solve1') (Done $ Solver "./inputs/02/1.txt" Day02.solve2')),
    (3, DaySolvers (Done $ Solver "./inputs/03/1.txt" Day03.solve1') (Done $ Solver "./inputs/03/1.txt" Day03.solve2')),
    (4, DaySolvers (Done $ Solver "./inputs/04/1.txt" Day04.solve1') (Done $ Solver "./inputs/04/1.txt" Day04.solve2')),
    (5, DaySolvers (Done $ Solver "./inputs/05/1.txt" Day05.solve1') (Done $ Solver "./inputs/05/1.txt" Day05.solve2')),
    (6, DaySolvers (Done $ Solver "./inputs/06/1.txt" Day06.solve1') (Done $ Solver "./inputs/06/1.txt" Day06.solve2')),
    (7, DaySolvers (Done $ Solver "./inputs/07/1.txt" Day07.solve1') (Done $ Solver "./inputs/07/1.txt" Day07.solve2')),
    (8, DaySolvers (Done $ Solver "./inputs/08/1.txt" Day08.solve1') (Done $ Solver "./inputs/08/1.txt" Day08.solve2')),
    (9, DaySolvers (Done $ Solver "./inputs/09/1.txt" Day09.solve1') (Done $ Solver "./inputs/09/1.txt" Day09.solve2')),
    (10, DaySolvers (Done $ Solver "./inputs/10/1.txt" Day10.solve1') (Done $ Solver "./inputs/10/1.txt" Day10.solve2')),
    (11, DaySolvers (Done $ Solver "./inputs/11/1.txt" Day11.solve1') (Done $ Solver "./inputs/11/1.txt" Day11.solve2')),
    (12, DaySolvers (Done $ Solver "./inputs/12/1.txt" Day12.solve1') (Done $ Solver "./inputs/12/1.txt" Day12.solve2')),
    (13, DaySolvers NotDone NotDone),
    (14, DaySolvers NotDone NotDone),
    (15, DaySolvers NotDone NotDone),
    (16, DaySolvers NotDone NotDone),
    (17, DaySolvers NotDone NotDone),
    (18, DaySolvers NotDone NotDone),
    (19, DaySolvers NotDone NotDone),
    (20, DaySolvers NotDone NotDone),
    (21, DaySolvers NotDone NotDone),
    (22, DaySolvers NotDone NotDone),
    (23, DaySolvers NotDone NotDone),
    (24, DaySolvers NotDone NotDone),
    (25, DaySolvers NotDone NotDone)]

runSolver filename solver = do
  fileContents <- readFileText filename
  print (solver fileContents)

solveKnownDayPart :: Int -> SolvedState Solver -> IO ()
solveKnownDayPart part maybeSolver = do
  putText (System.Console.Pretty.style Italic "Part " <> show part <> ": " :: Text)
  case maybeSolver of
    Done (Solver filename solver) -> do
      fileContents1 <- readFileText filename
      let solution1 = solver fileContents1
      printColouredMaybe solution1
    NotDone -> putTextLn (color Red "Not implemented" :: Text)

solveKnownDay :: Day -> DaySolvers SolvedState -> IO ()
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
