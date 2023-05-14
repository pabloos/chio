


{-
    AST.hs defines the specific AST used by the semantics phase
-}

module Frontend.Semantic.AST where

import qualified Data.Map as Map

import Frontend.GenericAST
import Operations
import Types

data TypedExpr = Arith ArithOperation TypedExpr TypedExpr
               | Logic LogicOperation TypedExpr TypedExpr
               | Comp CompOperation TypedExpr TypedExpr
               | Not TypedExpr
               | Negate TypedExpr
               | Lit Literal
               | SymbolRef String TypeValue
               | Call String [TypedExpr] TypeValue
               deriving (Show, Eq)

instance Typed TypedExpr where
    typeOf Arith {} = IntType
    typeOf Logic {} = BoolType
    typeOf Comp {} = BoolType
    typeOf Not {} = BoolType
    typeOf Negate {} = IntType
    typeOf (Lit (IntVal _)) = IntType
    typeOf (Lit (BoolVal _)) = BoolType
    typeOf (Lit (StringVal _)) = StringType
    typeOf (SymbolRef _ type_) = type_
    typeOf (Call _ _ type_) = type_

type TypedStatement   = Statement  TypedExpr
type TypedFunction    = Function TypedExpr
type TypedAST         = GenericAST TypedExpr