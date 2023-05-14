

{-
    Parser.hs contains the parser definition and some primitives
    defined upon the lexeme and some other ones of megaparsec
-}

module Frontend.Parser.Parser where

import Prelude hiding (many)

import Data.Void ( Void )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- blank space: spaces and comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- symbol (every lexem that isn't included amog the reserved words)
symbol :: String -> Parser String
symbol = L.symbol sc

-- reserved words
reservedWords :: [String]
reservedWords = ["fun", "if", "else", "true", "false", "while", "return", "int", "bool", "string", "{", "}", "->"]

-- every lexeme has to be followed by a blank sppace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens, braces, quotes :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (reserved "{") (reserved "}")
quotes = between (char '"') (char '"')

comma :: Parser ()
comma = reserved ","

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

natural :: Parser Integer
natural = lexeme L.decimal