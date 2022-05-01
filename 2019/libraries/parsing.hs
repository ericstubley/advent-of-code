module Parsing ( Parser
               , parseInput 
               , natural
               , integer
               , module Text.Megaparsec
               , module Text.Megaparsec.Char) where



import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO



type Parser = Parsec Void String


parseInput :: String -> Parser a -> IO (Maybe a)
parseInput filename p = do
    raw <- readFile filename
    let result = runParser p "" raw
    case result of
        (Left err) -> (print err) >> return Nothing
        (Right r)  -> return (Just r)


-- common parsing tasks?
-- signed integers
natural :: Parser Int
natural = L.decimal


integer :: Parser Int
integer = L.signed space L.decimal
-- lists of things
-- grids