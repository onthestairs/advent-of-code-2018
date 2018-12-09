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


-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe gameParser
solve2' = solve2 <<$>> parseMaybe gameParser

data Game = Game {
  numberOfPlayers :: Int,
  numberOfMarbles :: Int
} deriving (Show)

gameParser :: Parser (Game)
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
updateScore delta (Player score) = Player (score + delta)
data GameState = GameState {
  _players :: PL.PointedList Player,
  _board :: PL.PointedList Int,
  _marblesInPlay :: [Marble]
} deriving (Show)

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
playGame gameState = case _marblesInPlay nextState of
  [] -> nextState
  _ -> playGame $ (rotatePlayers nextState)
  where
    nextState = doTurn gameState

pointedListToNonEmpty :: PL.PointedList a -> NonEmpty a
pointedListToNonEmpty pl = view PL.focus pl :| (view prefix pl <> view suffix pl)


gameToInitialGameState (Game numberOfPlayers numberOfMarbles) =  GameState initialPlayers initialBoard initialMarblesInPlay
  where
    initialPlayers = foldl' (\ps _ -> PL.insert (Player 0) ps) (PL.singleton (Player 0)) [1..(numberOfPlayers-1)]
    initialBoard = PL.singleton 0
    initialMarblesInPlay = [1..numberOfMarbles]

solve1 :: Game -> Int
solve1 game = maximum1 $ fmap _score $ pointedListToNonEmpty $ _players $ playGame (gameToInitialGameState game)

solve2 :: Game -> Int
solve2 (Game numberOfPlayers numberOfMarbles) = maximum1 $ fmap _score $ pointedListToNonEmpty $ _players $ playGame initialState
  where
    initialState = gameToInitialGameState (Game numberOfPlayers (100*numberOfMarbles))
