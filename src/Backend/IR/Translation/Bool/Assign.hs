

{-
    Assign.hs describes how a boolean expression is assigned to a symbol
-}

module Backend.IR.Translation.Bool.Assign where

import Types (TypeValue(BoolType))
import Frontend.Semantic.AST
import Backend.IR.Translator
import Backend.IR.Translation.Bool.Conditionals
import Backend.IR.Spec.Instructions (Instruction(Instruction, label, op), Operation (Copy))
import Backend.IR.Spec.Operand (Operand(Symbol))
import Backend.IR.Translation.Bool.Encoding (true, false)

boolAssign :: String -> TypedExpr -> Translator ()
boolAssign name expr = do
    trueL <- getLabel
    falseL <- getLabel
    endLabel <- getLabel

    setLabels trueL falseL

    check expr

    ir Instruction{ label = trueL, op = setTrue name}
    jumpTo endLabel
    ir Instruction{ label = falseL, op = setFalse name}
    point endLabel

setTrue :: String -> Operation
setTrue name = Copy (Symbol name BoolType) true

setFalse :: String -> Operation
setFalse name = Copy (Symbol name BoolType) false