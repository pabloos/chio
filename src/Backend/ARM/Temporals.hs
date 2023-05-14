

{-
    Temporals.hs provides some primitives to manage temporals between operations
-}

module Backend.ARM.Temporals where

import Control.Monad.Writer ( MonadTrans(lift), MonadWriter(tell) )

import Backend.IR.Spec.Operand (Temporal)
import Backend.ARM.Translator (Translator)
import Backend.ARM.Context.Allocator (getRegOf, leave)
import Backend.ARM.Spec.Registers (Register)
import Backend.ARM.Spec.Operations (pushRegisters, popRegisters)


-- result sets a temporal as a register that wil be readed later
result :: Temporal -> Translator Register
result temp = lift $ getRegOf temp -- get a new register

-- operand sets a temporal as a register that is released after read 
operand :: Temporal -> Translator Register
operand temp = do -- get and release
    reg <- lift $ getRegOf temp
    lift $ leave temp
    return reg

-- functions to keep the temporal registers between function calls
-- in order to not be overwritten within
saveRegisters :: Translator ()
saveRegisters = tell [pushRegisters]

restoreRegisters :: Translator ()
restoreRegisters = tell [popRegisters]