

{-
    Program.hs describe how the ARM asm file contains 
        - a text section (code)
        - and a static data section
-}

module Backend.ARM.Spec.Program where
    
import Backend.ARM.Spec.TextSection ( TextSection )
import Backend.ARM.Spec.DataSection ( DataSection )

data Program = Program{
    textsec :: TextSection,
    datasec :: DataSection
} deriving (Eq)

instance Show Program where
    show program = "\n\n" ++ show (textsec program) ++ "\n" ++ show (datasec program) ++ "\n"