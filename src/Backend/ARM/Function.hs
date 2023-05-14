{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.ARM.Function where

import Control.Monad.Writer ( MonadTrans(lift), MonadWriter(tell), when )

import Frontend.GenericAST (Param (name))
import qualified Backend.IR.Spec.Instructions as IR
import Backend.IR.Spec.Instructions ( Operation(CallRet, Call, Print), )
import Backend.ARM.Translator (Translator, asm)
import Backend.ARM.Spec.Instructions (Instruction (..), AlgOp (Add, Sub), Operand (ImmNum), MemSource (Index))
import Backend.ARM.Context.Frame.Frame (Parameter(Pos, Reg), size)
import Backend.Label (nolabel, Label (Label))
import Backend.ARM.Context.Allocator (getSymbolPos, getFrame)
import Backend.ARM.Context.Context (getName)
import Backend.ARM.Spec.Registers
import Backend.ARM.Context.Frame.Params
import Backend.ARM.Branches (setCheckpoint)

data FunctionType = Leaf | NoLeaf deriving (Eq, Show)

leafFunc :: [IR.Instruction] -> FunctionType
leafFunc xs
  | null xs = Leaf
  | any isNonLeaf xs = NoLeaf
  | otherwise = Leaf
  where
    isNonLeaf IR.Instruction {IR.op = Call {}} = True
    isNonLeaf IR.Instruction {IR.op = CallRet {}} = True
    isNonLeaf IR.Instruction {IR.op = Print {}} = True
    isNonLeaf _ = False

prologue :: FunctionType -> Translator ()
prologue functype = do
    frame <- lift getFrame
    name <- lift getName

    tell [Push $ if (functype == Leaf) && name /= "main" then [FP] else [FP, LR]]
    tell [Arith Add FP SP (ImmNum 0) nolabel]
    tell [Arith Sub SP SP (ImmNum (size frame)) nolabel]

loadParam :: Param -> Translator ()
loadParam param = do -- move param to var location
    paramLocation <- lift $ getParamPos (name param)
    varLocation   <- lift $ getSymbolPos (name param)

    case paramLocation of
      Reg reg -> tell [ -- register to mem
            Store reg (Index varLocation) nolabel
        ]
      Pos pos -> tell [ -- load the param from memory and store it in the var location in memory
            Load R0 (Index pos) nolabel,
            Store R0 (Index varLocation) nolabel
        ]

-- epilogue
epilogue :: FunctionType -> Translator ()
epilogue functype = do
    name <- lift getName

    setCheckpoint (name ++ "_end")

    tell [
            Arith Add SP FP (ImmNum 0) nolabel,
            if functype == Leaf && (name /= "main") then Pop [FP] else Pop [FP, LR]
        ]
    returnControl

grantControlTo :: String -> Translator ()
grantControlTo target = asm $ BL target

returnControl :: Translator ()
returnControl =  asm $ BX LR