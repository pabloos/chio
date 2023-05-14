

{-
    Frontend.hs defines the way syntactic and semantic analises conects,
    implementing a Compiler (Untyped AST -> Typed AST)
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.Frontend (compile) where

import Control.Monad.Except (runExceptT, ExceptT, MonadError(throwError))
import Control.Monad.Reader (Reader, MonadIO (liftIO), MonadTrans (lift), runReader, ask)

import Text.Megaparsec (errorBundlePretty, parse)

import Compiler (Compiler(compile))
import Frontend.Parser.AST (UntypedAST)
import Frontend.Semantic.AST (TypedAST)
import Frontend.Semantic.Analysis (analize)
import Frontend.Parser.Program (program)
import Backend.IR.Translator (Translator)

-- the frontend stage could throw an error and stop the compiler
-- this behaviour follows the Except Monad
-- Because it needs to access in each phase the source code
-- it keeps it inside a reader monad (within the monad transformer)
type Frontend = ExceptT String (Reader String)

instance Compiler String (Either String TypedAST) where
    compile = runReader (runExceptT analysis)

analysis :: Frontend TypedAST
analysis = syntax >>= semantics

syntax :: Frontend UntypedAST
syntax = do
    src <- ask
    case parse program "" src of
        Left eb -> throwError (errorBundlePretty eb)
        Right ast -> return ast

semantics :: UntypedAST -> Frontend TypedAST
semantics ast = do 
    src <- ask
    case analize src ast of
        Left eb -> throwError (errorBundlePretty eb)
        Right ast -> return ast