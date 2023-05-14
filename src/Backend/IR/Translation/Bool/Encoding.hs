

{-
    Encoding.hs describes how the true and false values 
    are encoded as IR numbers
-}

module Backend.IR.Translation.Bool.Encoding where

import Backend.IR.Spec.Operand (Operand(Number))

true, false :: Operand
true = Number 1
false = Number 0