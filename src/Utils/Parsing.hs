module Utils.Parsing where

import Relude

import System.IO (FilePath)
import Text.Megaparsec

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO (Either (ParseError Char Void) a)
parseFile filepath parser = do
    fileContents <- readFile filepath
    pure $ runParser parser filepath fileContents