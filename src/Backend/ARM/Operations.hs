

{- 
    Operations.hs provides the translation of arithmetic, logical and comparison operations
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.ARM.Operations where

import           Operations
import           Backend.Label
import qualified Backend.IR.Spec.Operand as IR
import           Backend.ARM.Translator (Translator, asm)
import           Backend.ARM.Spec.Instructions
import qualified Backend.ARM.Spec.Instructions as ARM
import           Backend.ARM.Temporals

-- based on: https://stackoverflow.com/questions/15959446/how-get-twos-complement-of-a-register-value-in-arm
neg :: IR.Temporal -> IR.Temporal -> Translator ()
neg temp1 temp2 = do
    r2 <- operand temp1
    r1 <- result temp2

    asm $ MVN r1 r2
    asm $ Arith ARM.Add r1 r1 (ImmNum 1) nolabel

-- operations
arithmetic :: Label -> ArithOperation -> IR.Temporal -> IR.Temporal -> IR.Temporal -> Translator ()
arithmetic label op t0 t1 t2 = do
    r1 <- operand t1
    r2 <- operand t2
    r0 <- result t0

    asm $ Arith (ir2asm op) r0 r1 (Reg r2) label

comp :: Label -> IR.Temporal -> IR.Temporal -> Translator ()
comp label t1 t2 = do
    r0 <- operand t1
    r1 <- operand t2

    asm $ Cmp r0 (Reg r1) label

comp2Int :: Label -> IR.Temporal -> IR.Operand -> Translator ()
comp2Int label t1 (IR.Number int) = do
    r0 <- operand t1

    asm $ Cmp r0 (ImmNum int) label

ir2asm :: ArithOperation -> ARM.ArithOp
ir2asm Operations.Add = ARM.Add
ir2asm Operations.Sub = ARM.Sub
ir2asm Operations.Mul = ARM.Mul