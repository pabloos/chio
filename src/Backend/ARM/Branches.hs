

{-
    Branches.hs provides translation methods to get asm branching
-}

module Backend.ARM.Branches where
    
import Operations (CompOperation (..))
import Backend.IR.Spec.Operand (Temporal)
import Backend.Label (Label (Label))
import Backend.ARM.Spec.Operations (branch, checkpoint)
import Backend.ARM.Spec.Instructions (Instruction(..), Operand (ImmNum))
import Backend.ARM.Temporals (operand)
import Backend.ARM.Translator (Translator, asm)
import Backend.ARM.Spec.Registers (Register(LR))

branchTo :: String -> Translator ()
branchTo target = asm $ branch target

branchIf :: CompOperation -> Label -> Translator ()
branchIf op label = asm $ asmJump op label

asmJump :: CompOperation -> Label -> Instruction
asmJump Eq (Label l) = BEQ l
asmJump Ne (Label l) = BNE l
asmJump Gt (Label l) = BGT l
asmJump Lt (Label l) = BLT l
asmJump Ge (Label l) = BGE l
asmJump Le (Label l) = BLE l

setCheckpoint :: String -> Translator ()
setCheckpoint name = asm $ checkpoint name