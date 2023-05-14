

{-
    Scope.hs defines the scope's stack behaviour
-}

module Frontend.Semantic.Symbol.Scope where

import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Frontend.Semantic.Symbol.Table

parentKey :: String
parentKey = ".."

push :: SymbolTable -> SymbolTable
push current = Map.singleton parentKey (Scope current)

pop :: SymbolTable -> SymbolTable
pop current = case fromJust $ Map.lookup parentKey current of (Scope x) -> x