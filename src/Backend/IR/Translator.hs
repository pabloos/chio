

{-
    Translator.hs defines the monad who translates the AST to IR
    with some funtions to manage labels and temporals
-}

module Backend.IR.Translator where

import Control.Arrow ((>>>))
import Control.Monad.Trans.Writer ( WriterT, tell )
import Control.Monad.State ( MonadTrans(lift), State, gets, modify )

import Backend.Label (nolabel, Label, newLabel)
import Backend.IR.Spec.Instructions
import Backend.IR.Spec.Operand

data Counters = Counters {
    funcs :: Int,  -- to track globaly the number of functions in order to generate unique labels
    labels :: Int, -- to track localy (in each function) the number of labels to generate them
    temporals :: Int, -- same with temporals
    condLabels :: (Label, Label) -- each pair of labels involved in the conditional translation
}

type TranslatorState = State Counters

type Translator = WriterT [Instruction] TranslatorState

ir :: Instruction -> Translator ()
ir op = tell [op]

unlabeled :: Operation -> Translator ()
unlabeled op = ir Instruction{label = nolabel, op = op}

point :: Label -> Translator ()
point label = ir Instruction{label = label, op = None}

jumpTo :: Label -> Translator ()
jumpTo label = ir Instruction{label = nolabel, op = Jump label}

getLabel :: Translator Label
getLabel = do
    lift $ modify (\s -> s { labels = labels s + 1 })

    fi <- lift $ gets funcs
    li <- lift $ gets labels

    return $ newLabel fi li

newTemp :: Translator Temporal
newTemp = do
    lift $ modify (\s -> s { temporals = temporals s + 1 })

    ti <- lift $ gets temporals

    return $ Temporal ti

setLabels :: Label -> Label -> Translator ()
setLabels mid end = lift $ modify (\ctx -> ctx{condLabels = (mid, end)})

thenLabel :: Translator Label
thenLabel = lift $ gets (condLabels >>> fst)

elseLabel :: Translator Label
elseLabel = lift $ gets (condLabels >>> snd)