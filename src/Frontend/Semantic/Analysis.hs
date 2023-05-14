module Frontend.Semantic.Analysis where

import Data.Map
import Data.Foldable

import Control.Monad.Except
import Control.Monad.Trans.State

import Frontend.Parser.AST ( UntypedAST )
import Frontend.Semantic.AST ( TypedAST )
import Frontend.Semantic.Error.Report
import Frontend.Semantic.Analizer
import Frontend.Semantic.Analizer.MainChecker
import Frontend.Semantic.Analizer.SubprogramChecker
import Frontend.Semantic.State
import Frontend.Semantic.SymbolTable (addFunction)

analize :: String -> UntypedAST -> Either SemanticReport TypedAST
analize source ast = do
    let stateMonad = runExceptT (analysis ast)
    let startState = RecordState{code = source, returnType = Nothing, symbolTable = empty}
    evalState stateMonad startState

analysis :: UntypedAST -> Analizer TypedAST
analysis ast = do
    traverse_ addFunction ast
    checkMain ast
    traverse checkDefinition ast