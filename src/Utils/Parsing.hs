module Utils.Parsing where

import Relude

import System.IO (FilePath)
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Time.LocalTime
import Data.Time.Calendar

type Parser = Parsec Void Text

-- parseFile :: FilePath -> Parser a -> IO (Either (ParseError Char Void) a)
-- parseFile filepath parser = do
--     fileContents <- readFile filepath
--     pure $ runParser parser filepath fileContents

intParser :: Parser Int
intParser = L.decimal

integerParser :: Parser Integer
integerParser = L.decimal

signedIntParser :: Parser Int
signedIntParser = L.signed (hidden space) L.decimal


localTimeParser :: Parser LocalTime
localTimeParser = do
  year <- integerParser
  _ <- char '-'
  month <- intParser
  _ <- char '-'
  day <- intParser
  _ <- char ' '
  hour <- intParser
  _ <- char ':'
  minute <- intParser
  let date = fromGregorian year month day
  let time = TimeOfDay hour minute 0
  pure $ LocalTime date time
