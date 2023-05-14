

{-
    IR.hs describes how the Typed AST is transformed 
    in a collection of IR functions (synthesis)
-}

module Backend.IR.Translation.IR (synthesis) where

import Data.List (mapAccumL)

import Compiler (Compiler(compile))
import Frontend.Semantic.AST (TypedAST)
import Backend.IR.Spec.Instructions (Function)
import Backend.IR.Translation.Function ( translateDefinition )

synthesis :: TypedAST -> [Function]
synthesis ast = snd $ mapAccumL translateDefinition 0 ast