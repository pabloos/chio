

{-
    Build.hs describes how a stack frame is built
-}

module Backend.ARM.Context.Frame.Build where

import           Data.List (mapAccumL)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Control.Arrow ((>>>))

import            Types
import           Frontend.GenericAST
import qualified Frontend.Parser.AST as AST
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.ARM.Spec.Registers
import qualified Backend.ARM.Spec.Instructions as ARM
import           Backend.ARM.TypeSizes
import           Backend.ARM.Context.Frame.Frame hiding (size)
import           Backend.IR.Spec.Instructions (Instruction(Instruction, op))

buildFrame :: IR.Function -> Frame
buildFrame (ret, _, params, stmts) = do
    let (paramsRes, index) = buildParams params

    let retLen = if isJust ret then 4 else 0

    let accessSize = 0

    let carry = index + retLen + controlSize + accessSize + stateSize

    let (carry', paramsMap) = Map.mapAccum (\idx param -> (idx + size (type_ param), idx)) carry (Map.fromList $ zip (map name params) params)
    let (_, varsMap) = getVars' carry' stmts

    Frame{
        parameters = paramsRes,
        returnValue = Just $ RetReg R0,
        control = R8,
        access = Nothing, -- R9, -- fake
        state = LR,
        vars = Map.union paramsMap varsMap
    }

getVars' :: Int -> [Instruction] -> (Int, Map.Map String Int)
getVars' index (Instruction{ op = IR.Var name type_}:stmts) = do
    let index' = index + size type_

    let (index'', vars') = getVars' index' stmts

    (index'', Map.singleton name index' `Map.union` vars')
getVars' index (_:stmts) = getVars' index stmts
getVars' index [] = (index, Map.empty)

buildParams :: [Param] -> (Parameters, Int)
buildParams params = do
    let regParams = zip (map name params) [Reg R4, Reg R5, Reg R6, Reg R8] -- ARM: the first 4 args use to be passed through registers

    if length params <= 4 then (Map.fromList regParams, 0) else do -- if there's more args use memory
        let restNumber = length params - length regParams

        let tail = drop restNumber params

        let (i, memParams) = mapAccumL (\idx param -> (idx + size (type_ param), Pos idx)) 0 tail

        (Map.fromList $ regParams ++ regParams, i)