

{-
    Frame.hs describes the stack frame structure as a record datatype
    with his fields
-}

module Backend.ARM.Context.Frame.Frame where

import qualified Data.Map as Map

import Backend.ARM.Spec.Instructions 
import Backend.ARM.Spec.Registers (Register (R3))

------------------------- fp
-- parameters          --
-------------------------
-- return value        --
-------------------------
-- control (fp)        --
-------------------------
-- access (not using)  --
-------------------------
-- state (lr)          --
-------------------------
-- variables           --
------------------------- sp

data Frame = Frame {
    parameters :: Parameters,   -- <- fp
    returnValue :: Maybe ReturnVal,
    control :: Register,
    access :: Maybe Register,
    state :: Register,
    vars :: Vars                -- <- sp         
}

instance Eq Frame where
    f1 == f2 = [paramsSize $ parameters f1, returnSize $ returnValue f1, controlSize, accessSize $ access f1, stateSize, varsSize $ vars f1]
            == [paramsSize $ parameters f2, returnSize $ returnValue f2, controlSize, accessSize $ access f2, stateSize, varsSize $ vars f2]

instance Show Frame where
    show frame = "\n-----------------\n-- parameters x" ++ show (paramsSize (parameters frame))
              ++ "\n-----------------\n-- return x" ++ show (returnSize (returnValue frame))
              ++ "\n-----------------\n-- control x" ++ show controlSize 
              ++ "\n-----------------\n-- access x" ++ show (accessSize (access frame))
              ++ "\n-----------------\n-- state x" ++ show stateSize
              ++ "\n-----------------\n-- vars x" ++ show (varsSize (vars frame))
              ++ "\n-----------------\n"

data Parameter = Reg Register | Pos Int 
                deriving Show

data ReturnVal = RetReg Register | RetPos Int 
                deriving Show

type Vars = Map.Map String Int

type Parameters = Map.Map String Parameter

size :: Frame -> Int
size frame = sum [
                    paramsSize $ parameters frame, 
                    returnSize $ returnValue frame, 
                    controlSize, 
                    accessSize $ access frame, 
                    stateSize, 
                    varsSize $ vars frame
                ]

--------- 
-- sizes
--------- 
paramsSize :: Parameters -> Int
paramsSize params = Map.size params * 4

returnSize :: Maybe ReturnVal -> Int
returnSize (Just (RetPos n)) = 4
returnSize _ = 0

controlSize :: Int
controlSize = 4

accessSize :: Maybe Register -> Int
accessSize (Just _) = 4
accessSize Nothing = 0

stateSize :: Int
stateSize = 4

varsSize :: Vars -> Int
varsSize vars = Map.size vars * 4