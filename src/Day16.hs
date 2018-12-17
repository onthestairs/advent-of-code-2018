{-# LANGUAGE TemplateHaskell #-}

module Day16(
    solve1',
    solve2',
) where

import Relude hiding (many)

import Control.Applicative.Combinators hiding (sepBy, many, count, someTill)
import Control.Lens
import Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils.List
import Utils.Parsing

data Opcode =
  Addr |
  Addi |
  Mulr |
  Muli |
  Banr |
  Bani |
  Borr |
  Bori |
  Setr |
  Seti |
  Gtir |
  Gtri |
  Gtrr |
  Eqir |
  Eqri |
  Eqrr deriving (Show, Eq, Ord, Enum)

allOpcodes :: [Opcode]
allOpcodes = enumFromTo Addr Eqrr

type Register = Map.Map Int Int
data Value = Literal Int | Lookup Int
data Instruction = Instruction {
  _opcodeIdentifier :: Int,
  _a :: Int,
  _b :: Int,
  _c :: Int
} deriving (Show)
makeLenses ''Instruction
data Sample = Sample {
  _before :: Register,
  _instruction :: Instruction,
  _after :: Register
} deriving (Show)
makeLenses ''Sample

-- end to end solving functions
solve1' = solve1 <<$>> parseMaybe samplesAndInstructionsParser
solve2' = solve2 <=< parseMaybe samplesAndInstructionsParser

listToRegister rs = Map.fromList (zip [0..] rs)

intListParser :: Parser [Int]
intListParser = between (char '[') (char ']') (sepBy intParser (string ", "))

instructionParser :: Parser Instruction
instructionParser = do
  op <- intParser
  _ <- char ' '
  a <- intParser
  _ <- char ' '
  b <- intParser
  _ <- char ' '
  c <- intParser
  pure $ Instruction op a b c

sampleParser :: Parser Sample
sampleParser = do
  _ <- string "Before: "
  rsBefore <- intListParser
  _ <- newline
  instruction <- instructionParser
  _ <- newline
  _ <- string "After:  "
  rsAfter <- intListParser
  pure $ Sample (listToRegister rsBefore) instruction (listToRegister rsAfter)

samplesParser :: Parser [Sample]
samplesParser = sepBy sampleParser (count 2 newline)

instructionsParser :: Parser [Instruction]
instructionsParser = sepBy instructionParser newline

samplesAndInstructionsParser = do
  samples <- someTill (sampleParser <* (count 2 newline)) (count 2 newline)
  -- count 4 newline
  -- newline
  -- string "----"
  -- newline
  instructions <- instructionsParser
  pure $ (samples, instructions)

opOverValues :: Register -> (Int -> Int -> Int) -> Value -> Value -> Int -> Register
opOverValues r f (Literal n) (Literal m) c = Map.insert c (f n n) r
opOverValues r f (Lookup n) (Literal m) c = case Map.lookup n r of
  Just n' -> Map.insert c (f n' m) r
  Nothing -> r
opOverValues r f (Literal n) (Lookup m) c = case Map.lookup m r of
  Just m' -> Map.insert c (f n m') r
  Nothing -> r
opOverValues r f (Lookup n) (Lookup m) c = case (Map.lookup n r, Map.lookup m r) of
  (Just n', Just m') -> Map.insert c (f n' m') r
  _ -> r

predToInt :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
predToInt p = (\a b -> if p a b then 1 else 0)

runOpcode :: Register -> Opcode -> Int -> Int -> Int -> Register
runOpcode r Addr a b c = opOverValues r (+) (Lookup a) (Lookup b) c
runOpcode r Addi a b c = opOverValues r (+) (Lookup a) (Literal b) c
runOpcode r Mulr a b c = opOverValues r (*) (Lookup a) (Lookup b) c
runOpcode r Muli a b c = opOverValues r (*) (Lookup a) (Literal b) c
runOpcode r Banr a b c = opOverValues r (.&.) (Lookup a) (Lookup b) c
runOpcode r Bani a b c = opOverValues r (.&.) (Lookup a) (Literal b) c
runOpcode r Borr a b c = opOverValues r (.|.) (Lookup a) (Lookup b) c
runOpcode r Bori a b c = opOverValues r (.|.) (Lookup a) (Literal b) c
runOpcode r Setr a b c = opOverValues r const (Lookup a) (Literal b) c
runOpcode r Seti a b c = opOverValues r const (Literal a) (Literal b) c
runOpcode r Gtir a b c = opOverValues r (predToInt (>)) (Literal a) (Lookup b) c
runOpcode r Gtri a b c = opOverValues r (predToInt (>)) (Lookup a) (Literal b) c
runOpcode r Gtrr a b c = opOverValues r (predToInt (>)) (Lookup a) (Lookup b) c
runOpcode r Eqir a b c = opOverValues r (predToInt (==)) (Literal a) (Lookup b) c
runOpcode r Eqri a b c = opOverValues r (predToInt (==)) (Lookup a) (Literal b) c
runOpcode r Eqrr a b c = opOverValues r (predToInt (==)) (Lookup a) (Lookup b) c

possibleOpcodes (Sample before (Instruction _ a b c) after) = Set.filter (\opcode -> runOpcode before opcode a b c == after) (Set.fromList allOpcodes)

solve1 = length . filter (\n -> n >= 3) . map (Set.size . possibleOpcodes) . fst

makePossibilitiesMap :: [Sample] -> Map.Map Int (Set.Set Opcode)
makePossibilitiesMap samples = foldl' removeImpossibilities allPossibitiesMap samples
  where
    allPossibitiesMap = Map.fromSet (Set.fromList . const allOpcodes) (Set.fromList [0..15])
    removeImpossibilities possibilitiesMap sample = Map.adjust (\possibilities -> Set.intersection possibilities (possibleOpcodes sample)) (view (instruction . opcodeIdentifier) sample) possibilitiesMap

converge f x = let x' = f x in if x == x' then x else converge f x'

data Possibilities a = Fixed a | Choices (Set.Set a) deriving (Eq, Ord, Show)
hasOnlyOneChoice :: Possibilities a -> Bool
hasOnlyOneChoice (Fixed _) = False
hasOnlyOneChoice (Choices s) = Set.size s == 1

simplifyPossibilitiesMap :: (Ord k, Ord v) => Map.Map k (Possibilities v) -> Map.Map k (Possibilities v)
simplifyPossibilitiesMap pm = case Map.lookupMin $ Map.filter hasOnlyOneChoice pm of
  Just (k, Choices cs) -> case Set.lookupMin cs of
    Just v -> Map.insert k (Fixed v) (Map.map deleteChoice pm)
      where
        deleteChoice (Choices cs') = Choices $ Set.delete v cs'
        deleteChoice (Fixed x) = Fixed x
    Nothing -> pm
  Nothing -> pm

solveConstraints :: (Ord k, Ord v) => Map.Map k (Set.Set v) -> Maybe (Map.Map k v)
solveConstraints pm = Map.fromList <$> mapM extractValue (Map.toList converged)
  where
    converged = converge simplifyPossibilitiesMap (Map.map Choices pm)
    extractValue (k, (Fixed v)) = Just (k, v)
    extractValue (k, _) = Nothing

runInstructions :: Map.Map Int Opcode -> Register -> [Instruction] -> Maybe Register
runInstructions opcodeIndex r is = foldlM folder r is
  where
    folder r' (Instruction op a b c) = do
      opcode <- Map.lookup op opcodeIndex
      pure $ runOpcode r' opcode a b c

solve2 (samples, instructions) = do
  opcodeMap <- solveConstraints (makePossibilitiesMap samples)
  finalRegistry <- runInstructions opcodeMap Map.empty instructions
  Map.lookup 0 finalRegistry

