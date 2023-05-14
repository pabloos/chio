

{-
    Translator.hs describes a Writer monad that compiles the IR code into arm assembly
-}

module Backend.ARM.Translator where

import Control.Monad.Trans.Writer (WriterT, tell)

import Backend.ARM.Spec.Instructions (Instruction)
import Backend.ARM.Context.Context (ContextState)

type Translator = WriterT [Instruction] ContextState

-- semantic synonym for tell
asm :: Instruction -> Translator ()
asm op = tell [op]