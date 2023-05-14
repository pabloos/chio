

{- 
    Operations.hs contains all the operations implemented natively. This covers
        - arithmetic (+, -, *)
        - logic      (&&, ||)
        - Comparison (==, !=, >, <, >=, <=)
-}

module Operations where

data ArithOperation = Add | Sub | Mul deriving (Show, Eq)

data LogicOperation = And | Or deriving (Show, Eq)

data CompOperation = Eq | Ne | Gt | Lt | Ge | Le deriving (Show, Eq)

-- in order to translate to assembly languages, it's a common practive to use the
-- inverse operation inside conditional branching
inverse :: CompOperation -> CompOperation
inverse Eq = Ne
inverse Ne = Eq
inverse Gt = Le
inverse Lt = Ge
inverse Le = Gt
inverse Ge = Lt