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

-- digitsToInt ::

-- signedIntParser :: Parser Int
-- signedIntParser = do
--     multiplier <- (string "+" *> 1) <|> (string "-" *> (-1))
--     digits <- some digitChar
--     pure $ (multiplier * digits)

-- lexeme          = L.lexeme spaceConsumer
-- integer         = L.decimal
-- signedIntParser = L.signed L.space integer

signedIntParser :: Parser Int
signedIntParser = L.signed (hidden space) L.decimal