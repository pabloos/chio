

{-
    Instructions.hs provides all the ARMv7 instructions used to translate
-}

module Backend.ARM.Spec.Instructions where

import Backend.Label
import Backend.ARM.Spec.Registers (Register)
import Data.List (intercalate)

data AlgOp = Add | Sub | Mul deriving (Eq)

instance Show AlgOp where
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"

data Instruction = HeadComment String
                | B String
                | BEQ String
                | BNE String
                | BGT String
                | BLT String
                | BGE String
                | BLE String
                | BL String
                | BX Register
                | Nop Label
                | Mov Register Operand Label
                | MVN Register Register
                | Load Register MemSource Label
                | LoadByte Register MemSource Label
                | Store Register MemSource Label
                | Arith AlgOp Register Register Operand Label
                | Cmp Register Operand Label
                | Push [Register]
                | Pop [Register]
                | SWI Int -- signal
                deriving (Eq)

instance Show Instruction where
    show ord = op ++ "\n"
        where op = case ord of
                HeadComment comment -> "\n\t// " ++ comment

                B target -> "\tb " ++ target
                BL  target -> "\tbl " ++ target
                BX  reg    -> "\tbx " ++ show reg
                BEQ target -> "\tbeq " ++ target
                BNE target -> "\tbne " ++ target
                BGT target -> "\tbgt " ++ target
                BLT target -> "\tblt " ++ target
                BGE target -> "\tbge " ++ target
                BLE target -> "\tble " ++ target
            
                Nop label                    -> show label ++ "nop"

                Mov     reg op label         -> show label ++ "mov " ++ show reg ++ ", " ++ show op
                
                Load    reg index label      -> show label ++ "ldr "  ++ show reg ++ ", " ++ show index
                LoadByte reg src label       -> show label ++ "ldrb " ++ show reg ++ ", " ++ show src

                Store   reg index label      -> show label ++ "str " ++ show reg ++ ", " ++ show index 

                Arith alg res op1 op2 label  -> show label ++ show alg ++ " " ++ show res ++ ", " ++ show op1 ++ ", " ++ show op2

                Cmp     reg op label         -> show label ++ "cmp " ++ show reg ++ ", " ++ show op
                
                Push    regs                 -> "\tpush {" ++ intercalate ", " (fmap show regs) ++ "}"
                Pop     regs                 -> "\tpop {" ++ intercalate ", " (fmap show regs) ++ "}"
                SWI     num                  -> "\tswi " ++ show num
                MVN     reg1 reg2            -> "\tmvn " ++ show reg1 ++ ", " ++ show reg2

type Index = Int

data Operand = Reg Register
             | ImmNum Int
             | Mem Index
              deriving (Eq)

data MemSource = Index Int 
               | DataPointer String 
               | Direct Register -- [r1]
                deriving Eq

instance Show MemSource where
    show (Index i) = "[sp, #" ++ show i ++ "]"
    show (DataPointer l) = "=" ++ l
    show (Direct reg) = "[" ++ show reg ++ "]"

instance Show Operand where
    show (Reg i)    = show i
    show (ImmNum i) = "#" ++ show i
    show (Mem s)    = "[sp, " ++ show s ++ "]"