

{-
    Operand.hs describes the different operators involved in the IR spec
-}

module Backend.IR.Spec.Operand where 

import Types

data Operand = Symbol String TypeValue -- symbol reference (vars)
            | Number Int               -- integer literal
            | ConstString String       -- const string literal
            | Temp Temporal            -- temporal reference (partial expr result)
            deriving (Eq, Show)

newtype Temporal = Temporal Int deriving (Ord, Eq, Show)