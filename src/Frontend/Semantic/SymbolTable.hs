

module Frontend.Semantic.SymbolTable where

import qualified Data.Map           as Map
import           Control.Monad.State

import qualified Frontend.GenericAST as GAST
import qualified Frontend.Parser.AST as AST
import           Frontend.Semantic.State (AnalysisState, code, symbolTable)
import qualified Frontend.Semantic.Error.Report as Report
import           Frontend.Semantic.Analizer (Analizer)
import           Frontend.Semantic.Error.Errors
import           Frontend.Semantic.Symbol.Table
import           Frontend.Semantic.Symbol.Scope
import           Frontend.Semantic.Report (reportAt)

insert :: Int -> String -> Symbol -> Analizer ()
insert pos key symbol = do
    current <- gets symbolTable

    case Map.lookup key current of
        Nothing -> modify (\state -> state{ symbolTable = Map.insert key symbol current })
        _ -> reportAt pos SymbolAlreadyDefined

search :: Int -> String -> Analizer (Maybe Symbol)
search pos name = do
    current <- gets symbolTable

    case Map.lookup name current of
        Nothing -> case Map.lookup parentKey current of
            Just (Scope parent) -> do
                modify (\state -> state {symbolTable = parent})
                var <- search pos name
                modify (\state -> state {symbolTable = current})
                return var
            _ -> return Nothing
        symbol -> return symbol

addFunction :: AST.UntypedFunction -> Analizer ()
addFunction (GAST.Function name params ret _ pos) = unless (name == "main") $ insert pos name (Function (GAST.typeSignature ret) params)