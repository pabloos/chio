

{-
    Expressions.hs describes how non-boolean expressions are synthetized
-}

module Backend.IR.Translation.Expressions where

import           Frontend.GenericAST
import           Frontend.Semantic.AST
import           Backend.IR.Spec.Operand
import           Backend.IR.Translator ( unlabeled, Translator, newTemp )
import qualified Backend.IR.Spec.Instructions as IR

eval :: TypedExpr -> Translator Operand
eval (Arith op expr1 expr2) = do
    temp1 <- eval expr1
    temp2 <- eval expr2

    resultTemp <- newTemp

    unlabeled $ IR.Arithmetic op (Temp resultTemp) temp1 temp2

    return (Temp resultTemp)

eval (Negate expr) = do
    resultTemp <- newTemp

    exprTemp <- eval expr

    unlabeled $ IR.Negate (Temp resultTemp) exprTemp

    return (Temp resultTemp)

eval (Not expr) = do
    resultTemp <- newTemp

    exprTemp <- eval expr

    unlabeled $ IR.Not (Temp resultTemp) exprTemp

    return (Temp resultTemp)

eval (Lit x) = do
    resultTemp <- newTemp

    let lit = case x of
            IntVal i -> Number (fromIntegral i)
            BoolVal b -> Number (if b then 1 else 0)
            StringVal s -> ConstString s

    unlabeled $ IR.Copy (Temp resultTemp) lit

    return (Temp resultTemp)

eval (SymbolRef name type_) = do
    resultTemp <- newTemp

    unlabeled $ IR.Copy (Temp resultTemp) (Symbol name type_)

    return (Temp resultTemp)

eval (Call name args type_) = do
    argsTemps <- traverse eval args

    resultTemp <- newTemp

    unlabeled $ IR.CallRet (Temp resultTemp) argsTemps name

    return (Temp resultTemp)