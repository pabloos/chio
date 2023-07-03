

module Backend.IR.Translation.Statements where

import Data.Foldable ( traverse_ )

import           Types (TypeValue(BoolType), Typed (typeOf))
import           Frontend.GenericAST
import           Frontend.Semantic.AST
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.IR.Translator
import           Backend.IR.Spec.Operand ( Operand(Symbol) )
import           Backend.IR.Spec.Instructions (Operation(Copy))
import           Backend.IR.Translation.Bool.Conditionals (check)
import           Backend.IR.Translation.Bool.Assign
import           Backend.IR.Translation.Expressions ( eval )

translate :: TypedStatement -> Translator ()
translate (If cond then_ _) = do
    prevThen <- thenLabel
    prevEnd  <- elseLabel

    thenL <- getLabel
    endL  <- getLabel

    setLabels thenL endL

    check cond
    point thenL
    traverse_ translate then_
    point endL

    setLabels prevThen prevEnd

translate (IfElse cond then_ else_ _) = do
    prevThen <- thenLabel
    prevEnd  <- elseLabel

    thenL <- getLabel
    elseL <- getLabel
    endL  <- getLabel

    setLabels thenL elseL

    check cond
    point thenL
    traverse_ translate then_
    jumpTo endL
    point elseL
    traverse_ translate else_
    point endL

    setLabels prevThen prevEnd
    
translate (While cond then_ _) = do
    prevThen <- thenLabel
    prevEnd  <- elseLabel

    test_ <- getLabel
    loopL <- getLabel
    endL  <- getLabel

    setLabels loopL endL

    jumpTo test_
    point loopL

    traverse_ translate then_

    point test_
    check cond
    jumpTo loopL
    point endL        

    setLabels prevThen prevEnd
    
translate (Var name type_ expr _) = do
    exprTemp <- eval expr

    unlabeled $ IR.Var name type_
    unlabeled $ Copy (Symbol name type_) exprTemp

translate (Assign name expr _) =
    case expr of
      Logic {} -> boolAssign name expr
      Comp {} -> boolAssign name expr
      Not {} -> boolAssign name expr
      Lit (BoolVal _) -> boolAssign name expr
      SymbolRef _ BoolType -> boolAssign name expr
      Call _ _ BoolType -> boolAssign name expr
      _ -> do
        tmp <- eval expr
        unlabeled $ Copy (Symbol name (typeOf expr)) tmp

translate (Print expr _) = do
    exprTemp <- eval expr

    unlabeled $ IR.Print exprTemp

translate (CallStmt name args _) = do
    tempArgs <- traverse eval args

    unlabeled $ IR.Call tempArgs name

translate (Return expr _) =
    case expr of
        Nothing -> unlabeled $ IR.Return Nothing
        Just expr -> do
            exprTemp <- eval expr

            unlabeled $ IR.Return $ Just exprTemp