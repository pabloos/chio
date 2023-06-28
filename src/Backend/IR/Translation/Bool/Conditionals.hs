

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
import           Backend.IR.Spec.Operand
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.IR.Spec.Instructions (Operation(Compare, JumpIf, Jump))
import           Backend.IR.Translation.Expressions

-- shorcicuit
andCtx, orCtx :: TypedExpr -> Translator ()
andCtx (Lit (BoolVal True)) = return ()
andCtx (Lit (BoolVal False)) = getEndLabel >>= jumpTo
andCtx (Not expr) = flipLabels >> orCtx expr >> flipLabels
andCtx (Logic And left right) = andCtx left >> andCtx right
andCtx (Logic Or left right) = orCtx left >> andCtx right
andCtx (Comp op left right) = do
    e1 <- eval left
    e2 <- eval right

    end <- getEndLabel
    
    unlabeled $ Compare e1 e2 nolabel
    unlabeled $ JumpIf (inverse op) end
        
andCtx expr = do
    temp <- eval expr
    end <- getEndLabel

    unlabeled $ Compare temp (Number 1) nolabel
    unlabeled $ JumpIf Ne end

orCtx (Lit (BoolVal True)) = getMidLabel >>= jumpTo
orCtx (Lit (BoolVal False)) = return ()
orCtx (Not expr) = flipLabels >> andCtx expr >> flipLabels
orCtx (Logic And left right) = andCtx left >> andCtx right
orCtx (Logic Or left right) = orCtx left >> andCtx right
orCtx (Comp op left right) = do
    e1 <- eval left
    e2 <- eval right

    mid <- getMidLabel

    unlabeled $ Compare e1 e2 nolabel
    unlabeled $ JumpIf op mid

orCtx expr = do
    temp <- eval expr
    mid <- getMidLabel
    
    unlabeled $ Compare temp (Number 1) nolabel
    unlabeled $ JumpIf Eq mid

flipLabels :: Translator ()
flipLabels = do
    thenL <- getMidLabel
    endL  <- getEndLabel

    setLabels endL thenL