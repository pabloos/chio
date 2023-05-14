

module Backend.IR.Translation.Function where

import Control.Monad.State
import Control.Monad.Writer

import qualified Frontend.GenericAST as GAST
import           Frontend.Semantic.AST
import           Backend.Label
import           Backend.IR.Spec.Instructions (Operation(Compare, JumpIf, Jump, Copy, None), Function)
import           Backend.IR.Translation.Statements
import           Backend.IR.Translator (Counters(..))

translateDefinition :: Int -> TypedFunction -> (Int, Function)
translateDefinition i (GAST.Function name params ret body _) = do
    let writer = runWriterT (traverse translate body)
    let countersState = Counters { funcs = i, labels = 0, temporals = 0, condLabels = (nolabel, nolabel)}
    let stmts = concat $ evalState writer countersState

    (i + 1, (GAST.typeSignature ret, name, params, stmts))