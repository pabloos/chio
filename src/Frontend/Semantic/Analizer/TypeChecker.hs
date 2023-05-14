

module Frontend.Semantic.Analizer.TypeChecker where

import Data.Foldable

import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map

import Types

import           Frontend.GenericAST
import           Frontend.Parser.AST
import qualified Frontend.Parser.AST as AST
import qualified Frontend.Semantic.AST as TypedAST
import qualified Frontend.Semantic.Symbol.Table as Symbol
import           Frontend.Semantic.Analizer
import           Frontend.Semantic.State (symbolTable)
import           Frontend.Semantic.Error.Errors
import           Frontend.Semantic.Report (reportAt, report)
import           Frontend.Semantic.SymbolTable (search)

match :: (Located a, Typed b) => (a,  b, TypeValue) -> Analizer ()
match (l, t, tv) = when (typeOf t /= tv) $ reportAt (location l) TypeMissmatch

checkCardinality :: [Param] -> [UntypedExpr] -> Analizer ()
checkCardinality params args
    | length params == length args = return ()
    | length params <  length args = reportAt (location $ head args) TooManyArguments
    | otherwise                    = reportAt (location $ head args) TooFewArguments

returnMatch :: Int -> Maybe TypeValue -> Maybe UntypedExpr -> Analizer ()
returnMatch _   Nothing Nothing = return ()
returnMatch pos _       Nothing = reportAt pos ReturnTypeMissmatch
returnMatch pos Nothing _       = reportAt pos ReturnTypeMissmatch
returnMatch pos (Just type_) (Just expr) = do
                                            e <- typeExpr expr
                                            match (expr, e, type_)

typeExpr :: UntypedExpr -> Analizer TypedAST.TypedExpr
typeExpr (Arith op _ left right) = do
    e1 <- typeExpr left
    match (left, e1, IntType)

    e2 <- typeExpr right
    match (right, e2, IntType)

    return $ TypedAST.Arith op e1 e2

typeExpr (Logic op _ left right) = do
    e1 <- typeExpr left
    match (left, e1, BoolType)

    e2 <- typeExpr right
    match (right, e2, BoolType)

    return $ TypedAST.Logic op e1 e2

typeExpr (Comp op _ left right) = do
    e1 <- typeExpr left
    match (left, e1, IntType)

    e2 <- typeExpr right
    match (right, e2, IntType)

    return $ TypedAST.Comp op e1 e2

typeExpr (Call pos name args) = do
    found <- search pos name

    case found of
        Just (Symbol.Function (Just returnType) params) -> do
            checkCardinality params args
            typedArgs <- traverse typeExpr args
            traverse_ match (zip3 args typedArgs (type_ <$> params))

            return $ TypedAST.Call name typedArgs returnType
        Just (Symbol.Function Nothing _) -> throwError =<< report pos TypeMissmatch
        Just _  -> throwError =<< report pos (SymbolNotFunction name)
        Nothing -> throwError =<< report pos (SymbolNotFound name)

typeExpr (Negate _ expr) = do 
    e <- typeExpr expr
    match (expr, e, IntType)

    return $ TypedAST.Negate e

typeExpr (Not _ expr) = do
    e <- typeExpr expr
    match (expr, e, BoolType)
    
    return $ TypedAST.Not e

typeExpr (Lit literal _) = return $ TypedAST.Lit literal

typeExpr (SymbolRef pos name) = do
    found <- search pos name

    case found of
        Just (Symbol.Variable type_) -> return $ TypedAST.SymbolRef name type_
        Just _  -> throwError =<< report pos (SymbolNotVariable name)
        Nothing -> throwError =<< report pos (SymbolNotFound name)