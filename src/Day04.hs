module Day04(
    solve1',
    solve2',
) where

import Relude
import Relude.Extra.Foldable1

import qualified Data.Map as Map
import qualified Data.List as List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time.LocalTime
import Data.Time.Calendar

import Utils.Parsing
import Utils.Map

-- end to end solving functions
solve1' = solve1 <=< parseMaybe guardLogParser
solve2' = solve2 <=< parseMaybe guardLogParser

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

---

type Minute = Int
type SleepPattern = Map.Map Minute Int

data SleepInterval = SleepInterval {
  startMinute :: Minute,
  endMinute :: Minute
} deriving (Show)
minutesAsleep :: SleepInterval -> [Minute]
minutesAsleep (SleepInterval start end) = [start..end-1]
data GuardShift = GuardShift {
  _guardId :: GuardId,
  _sleepIntervals :: [SleepInterval]
} deriving (Show)

isShiftStart :: LogEntry -> Bool
isShiftStart (LogEntry  _ (StartsShift _)) = True
isShiftStart _ = False

splitLog :: [LogEntry] -> [[LogEntry]]
splitLog [] = []
splitLog (shiftStart@(LogEntry _ (StartsShift _)):log) = (shiftStart:shift):(splitLog rest)
  where (shift, rest) = List.span (not . isShiftStart) log

getEventMinute = todMin . localTimeOfDay
guardLogToIntervals :: [LogEntry] -> [SleepInterval]
guardLogToIntervals [] = []
guardLogToIntervals ((LogEntry asleepTime FallsAsleep):(LogEntry awakeTime WakesUp):rest) =
  (SleepInterval (getEventMinute asleepTime) (getEventMinute awakeTime):guardLogToIntervals rest)

shiftLogToShift :: [LogEntry] -> GuardShift
shiftLogToShift ((LogEntry _ (StartsShift guardId)):rest) = GuardShift guardId (guardLogToIntervals rest)

logToShifts :: [LogEntry] -> [GuardShift]
logToShifts log = map shiftLogToShift $ splitLog sortedLog
  where sortedLog = sortOn time log

shiftMinutesAsleep :: GuardShift -> [Minute]
shiftMinutesAsleep (GuardShift _ sleepIntervals) = concatMap minutesAsleep sleepIntervals

addShiftToSleepPattern :: GuardShift -> SleepPattern -> SleepPattern
addShiftToSleepPattern shift = insertManyWith (+) (shiftMinutesAsleep shift) 1

shiftsToSleepPatterns :: [GuardShift] -> Map.Map GuardId SleepPattern
shiftsToSleepPatterns = foldl' insertShift Map.empty
  where insertShift patterns shift@(GuardShift guardId _) = Map.alter (adjuster shift) guardId patterns
        adjuster :: GuardShift -> Maybe SleepPattern -> Maybe SleepPattern
        adjuster shift Nothing = Just $ addShiftToSleepPattern shift Map.empty
        adjuster shift (Just sleepPattern) = Just $ addShiftToSleepPattern shift sleepPattern

logToSleepPatterns = shiftsToSleepPatterns . logToShifts

sleepPatternToTotalMinutesAsleep :: SleepPattern -> Int
sleepPatternToTotalMinutesAsleep = sum . Map.elems

solve1 :: [LogEntry] -> Maybe Int
solve1 log = do
  let sleepPatterns = logToSleepPatterns log
  (guardId, _) <- maximumByValue (Map.map sleepPatternToTotalMinutesAsleep sleepPatterns)
  mostAsleepGuardSleepPattern <- Map.lookup guardId sleepPatterns
  (minute, _) <- maximumByValue mostAsleepGuardSleepPattern
  pure $ guardId * minute

sleepPatternToMaximumMinutesAsleep :: SleepPattern -> Maybe Int
sleepPatternToMaximumMinutesAsleep = viaNonEmpty maximum1 . Map.elems

solve2 :: [LogEntry] -> Maybe Int
solve2 log = do
  let sleepPatterns = logToSleepPatterns log
  (guardId, _) <- maximumByValue (Map.map sleepPatternToMaximumMinutesAsleep sleepPatterns)
  maxMinuteAsleepGuardSleepPattern <- Map.lookup guardId sleepPatterns
  (minute, _) <- maximumByValue maxMinuteAsleepGuardSleepPattern
  pure $ guardId * minute