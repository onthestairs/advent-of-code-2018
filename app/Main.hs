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

type Day = Int
data Solver = forall a. (Show a) => Solver (IO (Either (Megaparsec.ParseError Char Void) a))
data DaySolvers = DaySolvers Solver Solver

solvers :: Map Day DaySolvers
solvers = fromList [
    -- (0, DaySolvers Day00.solve1' Day00.solve2'),
    (1, DaySolvers (Solver Day01.solve1') (Solver Day01.solve2')),
    (2, DaySolvers (Solver Day02.solve1') (Solver Day02.solve2'))]

solveKnownDay :: Day -> DaySolvers -> IO ()
solveKnownDay day (DaySolvers (Solver solver1) (Solver solver2)) = do
    putStrLn (System.Console.Pretty.style Underline ("Day " <> show day) :: Text)
    putStr (System.Console.Pretty.style Italic "Part 1: " :: Text)
    solution1 <- solver1
    printColouredEither solution1
    putStr (System.Console.Pretty.style Italic "Part 2: " :: Text)
    solution2 <- solver2
    printColouredEither solution2

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
