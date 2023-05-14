

{-
    Operations.hs provides some ready to use instructions to
    - push and pop free-to-use registers
    - syscalls used
    - function call (branching)
    - checkpoints to jump to
-}

module Backend.ARM.Spec.Operations where

import Backend.Label (nolabel, Label (Label))
import Backend.IR.Spec.Instructions (Operation(Jump))
import Backend.ARM.Spec.Instructions (Instruction (Push, Pop, SWI, Mov, BX, Nop, B), Operand (ImmNum))
import Backend.ARM.Spec.Registers (freeRegisters, Register (R0, LR), syscallRegister)

pushRegisters :: Instruction
pushRegisters = Push freeRegisters

popRegisters :: Instruction
popRegisters = Pop freeRegisters

-- supervisor
interruption :: Instruction
interruption = SWI 0

exitOption :: Instruction
exitOption = Mov syscallRegister (ImmNum 1) nolabel

writeOption :: Instruction
writeOption = Mov syscallRegister (ImmNum 4) nolabel

stdout :: Instruction
stdout = Mov R0 (ImmNum 1) nolabel

-- functions call
branch :: String -> Instruction
branch = B

-- labels
checkpoint :: String -> Instruction
checkpoint name = Nop (Label name)