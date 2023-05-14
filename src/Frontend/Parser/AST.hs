

{-
    AST.hs defines the specific AST used by the syntatic phase
-}

module Frontend.Parser.AST where

import Operations
import Frontend.GenericAST

data UntypedExpr = Arith ArithOperation Int UntypedExpr UntypedExpr
                 | Negate Int UntypedExpr
                 | Lit Literal Int
                 | SymbolRef Int String
                 | Call Int String [UntypedExpr]
                 | Not Int UntypedExpr
                 | Logic LogicOperation Int UntypedExpr UntypedExpr
                 | Comp CompOperation Int UntypedExpr UntypedExpr
                 deriving (Eq, Show)

type UntypedStatement   = Statement UntypedExpr
type UntypedFunction    = Function UntypedExpr
type UntypedAST         = GenericAST UntypedExpr

instance Located UntypedExpr where
    location (Arith _ i _ _) = i
    location (Negate i _) = i
    location (Lit _ i) = i
    location (SymbolRef i _) = i
    location (Call i _ _) = i
    location (Not i _) = i
    location (Logic _ i _ _) = i
    location (Comp _ i _ _) = i