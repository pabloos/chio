

{-
    Table.hs defines Symbol and SymbolTable datatypes
-}

module Frontend.Semantic.Symbol.Table where

import qualified Data.Map as Map

import Types

import qualified Frontend.GenericAST as GAST

data Symbol = Scope SymbolTable
            | Function ReturnType [GAST.Param]
            | Variable TypeValue
            deriving (Show, Eq)

type ReturnType = Maybe TypeValue

type SymbolTable = Map.Map String Symbol