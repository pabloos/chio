

module Frontend.Semantic.Analizer.MainChecker where

import Control.Arrow ((>>>))
import Data.List (find, null)
import Data.Maybe (isJust)

import Control.Monad.State
import Control.Monad.Except

import Frontend.GenericAST
import Frontend.Parser.AST

import Frontend.Semantic.Analizer hiding (Subprogram)
import Frontend.Semantic.Error.Errors
import Frontend.Semantic.Report (reportAt)

checkMain :: UntypedAST -> Analizer ()
checkMain ast = case find (funcName >>> (== "main")) ast of
    Nothing -> reportAt 0 NoMain
    Just main -> do
      unless (null (params main)) $ reportAt (parsePos (head (params main))) NotEmptyParams
      when (isJust ((ret >>> typeSignature) main)) $ reportAt ((ret >>> pos) main) TypedMain