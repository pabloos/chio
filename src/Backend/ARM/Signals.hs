

{-
    Signals.hs translates to asm interruptions
-}

module Backend.ARM.Signals where

import Backend.ARM.Translator (Translator, asm)
import Backend.ARM.Spec.Operations (interruption, exitOption)

setExit :: Translator ()
setExit = asm exitOption

interrupt :: Translator ()
interrupt = asm interruption