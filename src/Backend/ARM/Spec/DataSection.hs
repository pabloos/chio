

{-
    DataSection.hs describes the arm data section 
    and some functions to get the translation of
    constant string set and retrieve
-}

module Backend.ARM.Spec.DataSection where

import Backend.Label
import Backend.ARM.Spec.Registers (Register)
import Backend.ARM.Spec.Instructions (Instruction (Load), MemSource (DataPointer))

newtype DataSection = DataSection {strings :: [AsciiText]} deriving (Eq)

emptyData :: DataSection
emptyData = DataSection{ strings = [] }

instance Show DataSection where
    show section = ".section .data\n" ++ concatMap show (strings section)

data AsciiText = AsciiText {
    strLabel :: String,
    str :: String
} deriving Eq

instance Show AsciiText where
    show text = strLabel text ++ ":\n\t.asciz " ++ "\"" ++ str text ++ "\"" ++ "\n"

newConstStr :: String -> Int -> AsciiText
newConstStr str index = AsciiText{
    strLabel = "_str" ++ show index,
    str = str
}

loadFromData :: Register -> String -> Label -> Instruction
loadFromData reg name = Load reg (DataPointer name)