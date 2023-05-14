

{-
    Registers.hs defines how the registers 
    are related to IR temporals with a Map
-}

module Backend.ARM.Context.Registers where

import qualified Data.Map as Map

import Backend.IR.Spec.Operand
import Backend.ARM.Spec.Registers (Register, freeRegisters)

type RegisterSet = [Register]

type RegistersVault = (Map.Map Temporal Register, RegisterSet)

newRegSet :: RegistersVault
newRegSet = (Map.empty, freeRegisters)