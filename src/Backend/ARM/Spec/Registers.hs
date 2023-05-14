

{-
    Registers.hs describes all the possible registers used in our translation
    and specific use of some of them
-}

module Backend.ARM.Spec.Registers where

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | FP | SP | LR | PC deriving (Eq, Ord, Read)

instance Show Register where
    show R0 = "r0"
    show R1 = "r1"
    show R2 = "r2"
    show R3 = "r3"
    show R4 = "r4"
    show R5 = "r5"
    show R6 = "r6"
    show R7 = "r7"
    show R8 = "r8"
    show R9 = "r9"
    show R10 = "r10"
    show FP = "fp"
    show SP = "sp"
    show PC = "pc"
    show LR = "lr"

returnRegister, syscallRegister :: Register
returnRegister = R3
syscallRegister = R7

freeRegisters :: [Register]
freeRegisters = [R0, R1, R2]