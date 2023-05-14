

{-
    Copy.hs describe how to translate the copy ir instruction 
    to move, str and ldr arm instructions
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.ARM.Copy where
    
import Control.Monad.Writer

import Backend.Label
import qualified Backend.IR.Spec.Operand as IR
import Backend.ARM.Translator (Translator, asm)
import Backend.ARM.Temporals
import Backend.ARM.Spec.Registers
import Backend.ARM.Spec.Instructions
import Backend.ARM.Context.Allocator
import Backend.ARM.Constants (storeStr)

copy :: Label -> IR.Operand -> IR.Operand -> Translator ()
copy label (IR.Temp t1) (IR.Temp t2) = do -- mov r0 r1
    r2 <- operand t2
    r1 <- result t1

    asm $ Mov r1 (Reg r2) label

copy label (IR.Temp t1) (IR.Number i) = do -- mov r0 #100
    r1 <- result t1

    asm $ Mov r1 (ImmNum i) label

copy label (IR.Temp t1) (IR.ConstString str) = do
    strdata <- storeStr str

    reg <- result t1

    asm $ Load reg (DataPointer strdata) nolabel

copy label (IR.Temp t1) (IR.Symbol name type_) = do -- ldr r0 {a}
    r1 <- result t1

    index <- lift $ getSymbolPos name

    asm $ Load r1 (Index index) label

copy label (IR.Symbol name _) (IR.Temp temp) = do -- str r0 {a}
    index <- lift $ getSymbolPos name

    r1 <- operand temp

    asm $ Store r1 (Index index) label

copy label (IR.Symbol name _) (IR.Number num) = do
    index <- lift $ getSymbolPos name

    asm $ Mov R0 (ImmNum num) nolabel
    asm $ Store R0 (Index index) label

copy label (IR.Symbol name1 _) (IR.Symbol name2 _) = do -- {a} -> {b}
    index  <- lift $ getSymbolPos name1
    index2 <- lift $ getSymbolPos name2

    asm $ Load R0 (Index index) label
    asm $ Store R0 (Index index2) nolabel