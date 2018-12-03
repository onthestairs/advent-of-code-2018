module Utils.Parsing where

import Relude

import System.IO (FilePath)
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO (Either (ParseError Char Void) a)
parseFile filepath parser = do
    fileContents <- readFile filepath
    pure $ runParser parser filepath fileContents

signedIntParser :: Parser Int
signedIntParser = L.signed (hidden space) L.decimal