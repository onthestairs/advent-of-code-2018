{-# LANGUAGE TemplateHaskell #-}

module Day09(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

import Control.Lens
import qualified Data.List.PointedList.Circular as PL
import Data.List.PointedList (prefix, suffix)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Parsing

data Game = Game {
  _numberOfPlayers :: Int,
  _numberOfMarbles :: Int
} deriving (Show)
makeLenses ''Game

gameParser :: Parser Game
gameParser = do
  numberOfPlayers <- intParser
  _ <- string " players; last marble is worth "
  lastMarble <- intParser
  _ <- string " points"
  pure $ Game numberOfPlayers lastMarble

type Marble = Int
data Player = Player {
  _score :: Int
} deriving (Show)
makeLenses ''Player
updateScore delta player = over score ((+) delta) player

data GameState = GameState {
  _players :: PL.PointedList Player,
  _board :: PL.PointedList Int,
  _marblesInPlay :: [Marble]
} deriving (Show)
makeLenses ''GameState

playMarble :: Marble -> PL.PointedList Marble -> PL.PointedList Marble
playMarble m c = PL.insertLeft m (PL.moveN 2 c)

doMod23Turn :: GameState -> GameState
doMod23Turn (GameState players board (m:ms)) =
  case PL.deleteRight shiftedBoard of
    Nothing -> error "shouldn't reach here!"
    Just newBoard -> GameState newPlayers newBoard ms
  where
    shiftedBoard = PL.moveN (-7) board
    boardScore = view PL.focus shiftedBoard
    newPlayers = over PL.focus (updateScore (m + boardScore))  players

doTurn :: GameState -> GameState
doTurn gs@(GameState players board (m:ms)) =
  case m `mod` 23 of
    0 -> doMod23Turn gs
    _ -> let newBoard = playMarble m board in GameState players newBoard ms

rotatePlayers :: GameState -> GameState
rotatePlayers (GameState players board ms) = GameState (PL.next players) board ms

playGame :: GameState -> GameState
playGame gameState = case view marblesInPlay nextState of
  [] -> nextState
  _ -> playGame (rotatePlayers nextState)
  where
    nextState = doTurn gameState

pointedListToNonEmpty :: PL.PointedList a -> NonEmpty a
pointedListToNonEmpty pl = view PL.focus pl :| (view prefix pl <> view suffix pl)

gameToInitialGameState :: Game -> GameState
gameToInitialGameState (Game numberOfPlayers numberOfMarbles) =  GameState initialPlayers initialBoard initialMarblesInPlay
  where
    -- this is a bit awkward as we need to prove non-emptiness
    initialPlayers = foldl' (\ps _ -> PL.insert (Player 0) ps) (PL.singleton (Player 0)) [1..(numberOfPlayers-1)]
    initialBoard = PL.singleton 0
    initialMarblesInPlay = [1..numberOfMarbles]

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe gameParser
solve2' = solve2 <<$>> parseMaybe gameParser

solve1 :: Game -> Int
solve1 game = maximum1 $ fmap (view score) $ pointedListToNonEmpty $ view players $ playGame (gameToInitialGameState game)

solve2 :: Game -> Int
solve2 game = maximum1 $ fmap (view score) $ pointedListToNonEmpty $ view players $ playGame initialState
  where
    initialState = gameToInitialGameState (over numberOfMarbles ((*) 100) game)
