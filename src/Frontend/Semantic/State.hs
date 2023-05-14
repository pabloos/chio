

{-
    State.hs defines the state of the semantic analysis monad
    with its api for push and pop scopes and set the return type of the scope
-}

module Frontend.Semantic.State where

import           Control.Monad.State
import qualified Data.Map           as Map

import           Types ( TypeValue )
import qualified Frontend.Semantic.Symbol.Scope as Scope
import           Frontend.Semantic.Symbol.Table (SymbolTable)

type AnalysisState = State RecordState

data RecordState = RecordState {
    code :: String,
    returnType :: Maybe TypeValue,
    symbolTable :: SymbolTable
}

openScope :: AnalysisState ()
openScope = do 
    current <- gets symbolTable
    modify (\state -> state{ symbolTable = Scope.push current })

closeScope :: AnalysisState ()
closeScope = do 
    current <- gets symbolTable
    modify (\state -> state{ symbolTable = Scope.pop current })

setReturn :: Maybe TypeValue -> AnalysisState ()
setReturn type_ = modify (\state -> state {returnType = type_})