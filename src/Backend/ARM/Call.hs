

{-
    Call.hs provides methods to forking and return from IR functions 
-}

module Backend.ARM.Call where

import Control.Monad.Writer ( MonadTrans(lift), MonadWriter(tell) )
import Data.Foldable (traverse_)

import Frontend.GenericAST (name)
import Backend.ARM.Translator (Translator, asm)
import Backend.IR.Spec.Operand (Temporal, Operand (..))
import Backend.ARM.Context.Frame.Frame
import qualified Backend.ARM.Spec.Instructions as ARM
import Backend.ARM.Temporals
import Backend.Label
import Backend.ARM.Context.Allocator
import Backend.ARM.Branches
import Backend.ARM.Context.Context (getName)
import Backend.ARM.Spec.Registers
import Backend.ARM.Context.Frame.Params (getParamIndexFrom, getParamsOf)
import Backend.ARM.Spec.Instructions (MemSource(Index, DataPointer), Instruction (Push, Pop))
import Backend.ARM.Constants (storeStr)
import Backend.ARM.Function (grantControlTo)
import Backend.ARM.Spec.DataSection (loadFromData)

-- functions calls
arg :: String -> (String, Temporal) -> Translator ()
arg funcName (paramName, temp) = do
    op <- operand temp

    paramLocation <- lift $ getParamIndexFrom funcName paramName

    case paramLocation of
        Reg reg -> asm $ ARM.Mov   reg (ARM.Reg op) nolabel
        Pos pos -> asm $ ARM.Store op  (Index pos) nolabel

pushArgs :: String -> [Operand] -> Translator ()
pushArgs callee args = do
    params <- lift $ getParamsOf callee

    let mix = zipWith (\param (Temp temp) -> (name param, temp)) params args

    traverse_ (arg callee) mix

call :: String -> [Operand] -> Translator ()
call name args = do
    pushArgs name args
    grantControlTo name

callWithReturn :: String -> [Operand] -> Temporal -> Translator ()
callWithReturn name args temp = do
    saveRegisters

    pushArgs name args

    res <- operand temp

    grantControlTo name
    restoreRegisters

    asm $ ARM.Mov res (ARM.Reg returnRegister) nolabel

ret :: Maybe Operand -> Translator ()
ret src = do
    case src of
        Nothing -> tell []
        Just val -> case val of
            Number num -> asm $ returnMov (ARM.ImmNum num) nolabel
            Symbol name _ -> do
                pos <- lift $ getSymbolPos name
                asm $ ARM.Load returnRegister (Index pos) nolabel
            Temp temp -> do
                reg <- operand temp
                asm $ returnMov (ARM.Reg reg) nolabel
            ConstString str -> do
                strdata <- storeStr str

                asm $ loadFromData R0 strdata nolabel
                asm $ returnMov (ARM.Reg R0) nolabel

    gotoEnd

gotoEnd :: Translator ()
gotoEnd = do
    name <- lift getName

    branchTo (name ++ "_end")

returnMov :: ARM.Operand -> Label -> Instruction
returnMov = ARM.Mov returnRegister