

{-
        Type.hs defines how type values (int, bool, string) are parsed
-}

module Frontend.Parser.Type where

import Control.Applicative

import Types

import Frontend.Parser.Parser

typeValue :: Parser TypeValue
typeValue = (reserved "int" >> return IntType)
        <|> (reserved "bool" >> return BoolType)
        <|> (reserved "string" >> return StringType)