

{-
    Backend.hs defines the backend stage as a instance of the compiler typeclass
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Backend.Backend (compile) where

import Control.Arrow ((>>>))

import Compiler (Compiler (compile))

import           Frontend.Semantic.AST (TypedAST)
import           Backend.ARM.Spec.Program (Program)
import qualified Backend.IR.Translation.IR as IR
import qualified Backend.ARM.Compile as Arm

instance Compiler TypedAST Program  where
    compile :: TypedAST -> Program
    compile = IR.synthesis >>> Arm.compile