module Day04(
    solve1',
    solve2',
) where

import Relude

import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time.LocalTime
import Data.Time.Calendar

import Utils.Parsing

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe guardLogParser
solve2' = solve2 <<$>> parseMaybe guardLogParser

type GuardId = Int
data GuardEvent =
    StartsShift GuardId
  | FallsAsleep
  | WakesUp deriving (Show)

data LogEntry = LogEntry {
  time :: LocalTime,
  event :: GuardEvent
} deriving (Show)

guardStartParser :: Parser GuardEvent
guardStartParser = do
  _ <- string "Guard #"
  guardId <- intParser
  _ <- " begins shift"
  pure $ StartsShift guardId

eventParser :: Parser GuardEvent
eventParser = do
  event <- guardStartParser <|> (string "wakes up" $> WakesUp) <|> (string "falls asleep" $> FallsAsleep)
  pure $ event

guardLogEntryParser :: Parser LogEntry
guardLogEntryParser = do
  localTime <- between (char '[') (char ']') localTimeParser
  _ <- char ' '
  event <- eventParser
  pure $ LogEntry localTime event

guardLogParser :: Parser [LogEntry]
guardLogParser = sepBy guardLogEntryParser newline

type Minute = Int
data SleepState = Asleep Minute | Awake
data GuardsState = GuardsState {
  currentGuard :: GuardId,
  currentGuardState :: SleepState,
  guardSleepMinutes :: Map.Map GuardId (Map.Map Minute Int)
}

getMinute :: LocalTime -> Minute
getMinute = todMin . localTimeOfDay

type Counter a = Map.Map a Int
updateBy :: Ord a => Counter a -> a -> Int -> Counter a
updateBy c k n = case Map.lookup k c of
  Just _ -> Map.insertWith (+) k n c
  Nothing -> Map.insert k n c
emptyCounter = Map.empty

updateManyBy :: Ord a => Counter a -> [a] -> Int -> Counter a
updateManyBy c ks n = foldl' (\c' k -> updateBy c' k n) c ks

updateGuardState :: GuardsState -> LogEntry -> GuardsState
updateGuardState s (LogEntry _ (StartsShift newGuardId)) = s { currentGuard = newGuardId }
updateGuardState s (LogEntry localTime FallsAsleep) = s { currentGuardState = (Asleep (getMinute localTime)) }
updateGuardState s@(GuardsState guardId (Asleep minute) sleepMinutes) (LogEntry localTime WakesUp) = s { currentGuardState = Awake, guardSleepMinutes = newAsleepMap }
  where minutesAsleep = [minute..(getMinute localTime)-1]
        newAsleepMap = case Map.lookup guardId sleepMinutes of
          Just guardMap -> Map.adjust (\c -> updateManyBy c minutesAsleep 1) guardId sleepMinutes
          Nothing -> Map.insert guardId (updateManyBy emptyCounter minutesAsleep 1) sleepMinutes


findMostAsleepGuard :: Map.Map GuardId (Map.Map Minute Int) -> Maybe GuardId
findMostAsleepGuard m = safeHead $ map fst $ reverse $ sortOn snd $ Map.toList guardMinutesAsleep
  where guardMinutesAsleep :: Map.Map GuardId Int
        guardMinutesAsleep = Map.map (sum . Map.elems) m

findModalAsleepMinute :: (Map.Map Minute Int) -> Maybe Minute
findModalAsleepMinute m = safeHead $ map fst $ reverse $ sortOn snd $ Map.toList m

solve1 :: [LogEntry] -> Maybe Int
solve1 log = do
  guardId <- mostAsleepGuard
  guardSleepCounter <- Map.lookup guardId guardsSleepCounter
  minute <- findModalAsleepMinute guardSleepCounter
  pure $ guardId * minute
  where sortedLog = sortOn time log
        initialState = GuardsState 0 Awake Map.empty
        asleepMinutes = foldl' updateGuardState initialState sortedLog
        guardsSleepCounter = (guardSleepMinutes asleepMinutes)
        mostAsleepGuard = findMostAsleepGuard guardsSleepCounter

-- data ModalMinute = ModalMinute {
--   M
-- }

findModalMinute :: (Ord k, Ord v) => Map.Map k v -> Maybe (k, v)
findModalMinute m = safeHead $ reverse $ sortOn snd $ Map.toList m

findMaxModalMinute :: (Ord k, Ord v) => Map.Map k (l, v) -> Maybe (k, (l, v))
findMaxModalMinute m = safeHead $ reverse $ sortOn (snd . snd) $ Map.toList m

solve2 :: [LogEntry] -> Maybe (Int, Int, Int, Int)
solve2 log = do
  let guardModalMinutes = Map.mapMaybe id $ Map.map findModalMinute guardsSleepCounter
  (guardId, (minute, timesAsleep)) <- findMaxModalMinute guardModalMinutes
  -- guardSleepCounter <- Map.lookup guardId guardsSleepCounter
  -- minute <- findModalAsleepMinute guardSleepCounter
  pure $ (guardId, minute, timesAsleep, guardId * minute)
  where sortedLog = sortOn time log
        initialState = GuardsState 0 Awake Map.empty
        asleepMinutes = foldl' updateGuardState initialState sortedLog
        guardsSleepCounter = (guardSleepMinutes asleepMinutes)
        -- mostAsleepGuard = findMostAsleepGuard guardsSleepCounter