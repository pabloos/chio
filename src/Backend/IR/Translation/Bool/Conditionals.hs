

{-
    Conditionals.hs describes how the '&&' and '||' are translated sequentialy 
-}

module Backend.IR.Translation.Bool.Conditionals where

import           Operations
import           Types (TypeValue(BoolType))
import           Frontend.GenericAST
import           Frontend.Semantic.AST
import           Backend.Label
import           Backend.IR.Translator
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.IR.Spec.Instructions (Operation(Compare, JumpIf, Jump))
import           Backend.IR.Translation.Expressions
import           Backend.IR.Translation.Bool.Encoding (true)

check :: TypedExpr -> Translator ()
check (Logic And l r) = conjuntion l >> conjuntion r -- both must be true
check (Logic Or  l r) = disjuntion l >> conjuntion r -- if the first fails, second must be true
check cond            = conjuntion cond

conjuntion, disjuntion :: TypedExpr -> Translator ()    -- ∧, ∨
conjuntion (Lit (BoolVal True))  = return ()            -- static true: skip check
conjuntion (Lit (BoolVal False)) = jumpTo =<< elseLabel -- static false: jump to end
conjuntion (Not expr)            = swapLabels >> disjuntion expr >> swapLabels -- flip labels and op
conjuntion (Comp op left right)  = do
    e1 <- eval left
    e2 <- eval right

    elseL <- elseLabel
    
    unlabeled $ Compare e1 e2 nolabel
    unlabeled $ JumpIf (inverse op) elseL

conjuntion expr = do
    temp <- eval expr
    elseL <- elseLabel

    unlabeled $ Compare temp true nolabel
    unlabeled $ JumpIf Ne elseL

disjuntion (Lit (BoolVal True))  = jumpTo =<< thenLabel -- static true: jump to body
disjuntion (Lit (BoolVal False)) = return ()            -- static false: skip check
disjuntion (Not expr)            = swapLabels >> conjuntion expr >> swapLabels -- flip labels and op
disjuntion (Comp op left right)  = do
    e1 <- eval left
    e2 <- eval right

    then_ <- thenLabel

    unlabeled $ Compare e1 e2 nolabel
    unlabeled $ JumpIf op then_

disjuntion expr = do
    temp <- eval expr
    thenL <- thenLabel
    
    unlabeled $ Compare temp true nolabel
    unlabeled $ JumpIf Eq thenL

swapLabels :: Translator ()
swapLabels = do
    thenL <- thenLabel
    elseL <- elseLabel

    setLabels elseL thenL