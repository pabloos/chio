

{-
    Instructions.hs describes all the operations used in the IR and their structure
-}

module Backend.IR.Spec.Instructions where

import Types ( TypeValue )
import Operations ( ArithOperation, CompOperation )

import qualified Frontend.GenericAST as GAST
import qualified Frontend.Parser.AST as Parser
import qualified Frontend.Semantic.AST as AST
import           Backend.IR.Spec.Operand
import           Backend.Label

-- IR code are collected inside functions the same way our syntax does with statements
type Function = (Maybe TypeValue, String, [GAST.Param], [Instruction])

data Instruction = Instruction {
    label :: Label,
    op :: Operation
} deriving (Eq, Show)

data Operation = Var String TypeValue
               | Copy Operand Operand
               | Arithmetic ArithOperation Operand Operand Operand 
               | Compare Operand Operand Label 
               | JumpIf CompOperation Label
               | Not Operand Operand
               | Negate Operand Operand
               | Jump Label
               | Call [Operand] String 
               | CallRet Operand [Operand] String
               | Return (Maybe Operand) 
               | Print Operand
               | None
               deriving (Show, Eq)