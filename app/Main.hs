{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Relude

import qualified Text.Megaparsec as Megaparsec
import qualified Data.Map
import Options.Applicative
import System.Console.Pretty (Color (..), Style (..), color, style)

import qualified Day00
import qualified Day01
import qualified Day02

printColouredEither :: (Show a, Show b) => Either a b -> IO ()
printColouredEither (Left a) = putStrLn (color Red (show a) :: Text)
printColouredEither (Right b) = putStrLn (color Green (show b) :: Text)

printColouredMaybe :: (Show a) => Maybe a -> IO ()
printColouredMaybe Nothing  = putStrLn (color Red "Did not solve" :: Text)
printColouredMaybe (Just a) = putStrLn (color Green (show a) :: Text)

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
    (3, DaySolvers Nothing Nothing),
    (4, DaySolvers Nothing Nothing),
    (5, DaySolvers Nothing Nothing),
    (5, DaySolvers Nothing Nothing),
    (6, DaySolvers Nothing Nothing),
    (7, DaySolvers Nothing Nothing),
    (8, DaySolvers Nothing Nothing),
    (9, DaySolvers Nothing Nothing),
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

solveKnownDayPart :: Int -> Maybe Solver -> IO ()
solveKnownDayPart part maybeSolver = do
  putStr (System.Console.Pretty.style Italic "Part " <> show part <> ": " :: Text)
  case maybeSolver of
    Just (Solver filename solver) -> do
      fileContents1 <- readFile filename
      let solution1 = solver fileContents1
      printColouredMaybe solution1
    Nothing -> putStrLn (color Red "Not implemented" :: Text)

solveKnownDay :: Day -> DaySolvers Maybe -> IO ()
solveKnownDay day (DaySolvers solver1 solver2) = do
    putStrLn (System.Console.Pretty.style Underline ("Day " <> show day) :: Text)
    solveKnownDayPart 1 solver1
    solveKnownDayPart 2 solver2

solveAllDays = forM_ (Data.Map.assocs solvers) (uncurry solveKnownDay)

solveDay :: Day -> IO ()
solveDay day = case Data.Map.lookup day solvers of
    Nothing -> putStrLn ("Couldn't find solvers for day " <> show day :: Text)
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
