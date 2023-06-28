

{-
    Print.hs provides the primitives to translate the print statements 
    whose are mainly two:
        - a print function
        - its call
-}

module Backend.ARM.Print where

import           Backend.IR.Spec.Operand (Temporal, Operand (Temp))
import           Backend.Label (nolabel, Label (Label))
import           Backend.ARM.Translator (Translator, asm)
import           Backend.ARM.Spec.Registers
import qualified Backend.ARM.Spec.Instructions as ARM
import           Backend.ARM.Signals (interrupt)
import           Backend.ARM.Spec.Instructions (Instruction(HeadComment, BEQ), Operand (Reg, ImmNum))
import           Backend.ARM.Temporals (operand, result)
import           Backend.ARM.Spec.Operations (writeOption, stdout)
import           Backend.ARM.Function (grantControlTo, returnControl)
import           Backend.ARM.Branches (branchTo)

header :: Translator ()
header = asm $ HeadComment "print"

setStdout :: Translator ()
setStdout = asm stdout

setWrite :: Translator ()
setWrite = asm writeOption

printCall :: Temporal -> Translator ()
printCall temp = do
    reg <- operand temp
    asm $ ARM.Mov R0 (Reg reg) nolabel
    grantControlTo "_print"
        
printFunc :: Translator ()
printFunc = do
    header
    
    asm $ ARM.Mov R1 (Reg R0) (Label "_print")
    asm $ ARM.LoadByte R2 (ARM.Direct R1) (Label "_pstrt")
    asm $ ARM.Cmp R2 (ARM.ImmNum 0) nolabel
    asm $ BEQ "_pend"
    asm $ ARM.Arith ARM.Add R1 R1 (ImmNum 1) nolabel
    branchTo "_pstrt"
    asm $ ARM.Arith ARM.Sub R3 R1 (Reg R0) (Label "_pend")
    
    setWrite
    asm $ ARM.Mov R1 (Reg R0) nolabel
    setStdout
    asm $ ARM.Mov R2 (ARM.Reg R3) nolabel
    interrupt
    returnControl