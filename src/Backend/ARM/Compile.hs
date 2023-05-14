

{-
    Compile.hs describe the IR translation to ARM assembly
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.ARM.Compile (compile) where

import Data.Foldable ( traverse_ )
import Control.Monad.Writer ( MonadTrans(lift), WriterT(runWriterT), MonadWriter(tell) )
import Control.Monad.State ( evalState, runState, when )

import qualified Backend.IR.Spec.Instructions as IR
import Backend.Label ( Label(Label), nolabel )
import Backend.IR.Spec.Operand ( Operand(Temp, ConstString, Number) )

import Backend.ARM.Spec.Instructions ( Instruction(Nop, SWI, Mov), Operand (ImmNum) )
import Backend.ARM.Function ( leafFunc, prologue, loadParam, epilogue )
import Backend.ARM.Translator (Translator)
import Backend.ARM.Branches (branchTo, branchIf)
import Backend.ARM.Copy ( copy )
import Backend.ARM.Call ( call, callWithReturn, ret )
import Backend.ARM.Operations ( arithmetic, comp, comp2Int, neg )
import Backend.ARM.Context.Context (newCtx, getName, setName, Context (datasection))
import Backend.ARM.Spec.Program
import Backend.ARM.Spec.TextSection (TextSection(Insts))
import Backend.ARM.Spec.Registers (Register(R7))
import Backend.ARM.Signals (setExit, interrupt)
import Backend.ARM.Print (printCall, printFunc)
import Backend.ARM.Start (start)
import Backend.IR.Spec.Instructions (Instruction(label, op))
import Compiler (Compiler (compile))

instance Compiler [IR.Function] Program where
    compile irFuncs = do
        let ctx = newCtx irFuncs

        let funcs = traverse_ compileFunction irFuncs
        let state = runWriterT (start >> printFunc >> funcs)

        let (insts, ste) = runState state ctx

        Program {
            textsec = Insts (snd insts),
            datasec = datasection ste
        }

compileFunction :: IR.Function -> Translator ()
compileFunction (_, name, params, stmts) = do
    lift $ setName name

    let type_ = leafFunc stmts

    header
    prologue type_
    traverse_ loadParam params
    traverse_ statement stmts
    epilogue type_

header :: Translator ()
header = do
    label <- lift getName
    tell [Nop $ Label label]

statement :: IR.Instruction -> Translator ()
statement IR.Instruction{label = lbl, op = IR.Copy addr1 addr2} = copy lbl addr1 addr2
statement IR.Instruction{label = lbl, op = IR.Arithmetic op (Temp t1) (Temp t2) (Temp t3)} = arithmetic lbl op t1 t2 t3
statement IR.Instruction{label = lbl, op = IR.Compare (Temp t1) (Temp t2) target} = comp lbl t1 t2
statement IR.Instruction{label = lbl, op = IR.Compare (Temp t1) val@(Number _) _} = comp2Int lbl t1 val
statement IR.Instruction{label = lbl, op = IR.Jump (Label target)} = branchTo target
statement IR.Instruction{label = lbl, op = IR.JumpIf op target} = branchIf op target
statement IR.Instruction{label = lbl, op = IR.Return mayberet} = ret mayberet
statement IR.Instruction{label = lbl, op = IR.Print (Temp temp)} = printCall temp
statement IR.Instruction{label = lbl, op = IR.Call arg funcname} = call funcname arg
statement IR.Instruction{label = lbl, op = IR.CallRet (Temp temp) args funcname} = callWithReturn funcname args temp
statement IR.Instruction{label = lbl, op = IR.Var _ _} = return ()
statement IR.Instruction{label = lbl, op = IR.None} = tell [Nop lbl]
statement IR.Instruction{label = lbl, op = IR.Not (Temp temp1) (Temp temp2)} = error "cannot be"-- notOp temp1 temp2
statement IR.Instruction{label = lbl, op = IR.Negate (Temp temp1) (Temp temp2)} = neg temp1 temp2

statement _ = error "asm translation: unexpected entry statement"