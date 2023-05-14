

{-
    GenericAST.hs defines the general structure of the AST
    excluding the expressions (as it could be typed or untyped)

    It exposes the following constructions:
        - literals (integers, booleans and strings)
        - statements
        - subprogram, params and return signature
-}

module Frontend.GenericAST where

import Types ( TypeValue )

data Literal = IntVal Integer | BoolVal Bool | StringVal String
    deriving (Eq, Show)

data Statement a = If a [Statement a] Int
                 | IfElse a [Statement a] [Statement a] Int
                 | While a [Statement a] Int
                 | Var String TypeValue a Int -- int a = 1
                 | Assign String a Int        -- a = 1
                 | Print a Int
                 | CallStmt String [a] Int
                 | Return (Maybe a) Int
                 deriving (Show, Eq)

data ReturnSignature = ReturnSignature{
    typeSignature :: Maybe TypeValue,
    pos :: Int
} deriving (Show, Eq)

data Function a = Function{
    funcName :: String,
    params :: [Param],
    ret :: ReturnSignature,
    stmts :: [Statement a],
    position :: Int
} deriving (Show, Eq)

data Param = Param{
    name :: String,
    type_ :: TypeValue,
    parsePos :: Int
} deriving (Show, Eq)

type GenericAST a = [Function a]

-- Located typeclass to get the position of a construction
class Located a where
    location :: a -> Int