

module Frontend.Semantic.Analizer.SubprogramChecker where

import qualified Data.Map as Map
import           Data.Foldable
import           Control.Monad.State
import           Control.Monad.Except

import Types

import           Frontend.GenericAST
import           Frontend.Parser.AST
import qualified Frontend.Semantic.Symbol.Table as Symbol hiding (Subprogram)
import qualified Frontend.Semantic.Symbol.Scope as Scope
import           Frontend.Semantic.Analizer
import           Frontend.Semantic.Analizer.TypeChecker
import           Frontend.Semantic.AST
import           Frontend.Semantic.State
import           Frontend.Semantic.Error.Errors
import           Frontend.Semantic.SymbolTable (insert, search)
import           Frontend.Semantic.Report (report)

checkDefinition :: UntypedFunction -> Analizer TypedFunction
checkDefinition (Function funcName params returnT stmts pos) = do
    lift $ setReturn (typeSignature returnT)

    lift openScope

    traverse_ (\param -> insert (parsePos param) (name param) (Symbol.Variable $ type_ param)) params

    stmts1 <- traverse checkStatement stmts

    lift closeScope

    return $ Function funcName params returnT stmts1 pos

checkStatement :: UntypedStatement -> Analizer TypedStatement
checkStatement stmt = case stmt of
    (CallStmt name args pos) -> do
        found <- search pos name
        case found of
            (Just (Symbol.Function _ params)) -> do
                checkCardinality params args
                exprs <- traverse typeExpr args

                traverse_ match (zip3 args exprs (type_ <$> params))

                return $ CallStmt name exprs pos
            Just _  -> throwError =<< report pos (SymbolNotFunction name)
            Nothing -> throwError =<< report pos (SymbolNotFound name)

    (Var name type_ expr pos) -> do
        e <- typeExpr expr
        match (expr, e, type_)

        insert pos name (Symbol.Variable type_)

        return $ Var name type_ e pos

    (Assign name expr pos) -> do
        found <- search pos name
        case found of
            Just (Symbol.Variable type_) -> do
                e <- typeExpr expr
                match (expr, e, type_)

                return $ Assign name e pos
            Just _  -> throwError =<< report pos (SymbolNotVariable name)
            Nothing -> throwError =<< report pos (SymbolNotFound name)

    (Return expr pos) -> do
        funcType <- lift $ gets returnType
        returnMatch pos funcType expr

        exprs <- traverse typeExpr expr
        return $ Return exprs pos

    (While cond then_ pos) -> do
        e1 <- typeExpr cond
        match (cond, e1, BoolType)

        stmts <- checkScope then_
        return $ While e1 stmts pos

    (If cond then_ pos) -> do
        e1 <- typeExpr cond
        match (cond, e1, BoolType)

        stmts <- checkScope then_
        return $ If e1 stmts pos

    (IfElse cond then_ else_ pos) -> do
        e1 <- typeExpr cond
        match (cond, e1, BoolType)

        stmts1 <- checkScope then_
        stmts2 <- checkScope else_

        return $ IfElse e1 stmts1 stmts2 pos

    (Print expr pos) -> do
        e <- typeExpr expr
        return $ Print e pos

checkScope :: [UntypedStatement] -> Analizer [TypedStatement]
checkScope stmts = do
    lift openScope
    sts <- traverse checkStatement stmts
    lift closeScope
    return sts