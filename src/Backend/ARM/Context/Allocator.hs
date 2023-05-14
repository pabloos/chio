

module Backend.ARM.Context.Allocator where

import           Data.Maybe ( fromJust )
import qualified Data.Map as Map
import qualified Control.Monad.State as State

import qualified Backend.IR.Spec.Operand as Operand
import           Backend.ARM.Context.Frame.Frame
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.ARM.Context.Context
import           Backend.ARM.Spec.Registers (Register)

getRegOf :: Operand.Temporal -> ContextState Register
getRegOf temp = do
    regMap  <- State.gets (fst . registers)
    regList <- State.gets (snd . registers)

    if Map.member temp regMap then do   -- get the register (read from)
        return $ fromJust $ Map.lookup temp regMap
    else do
        ctx <- State.get
                                     -- set and get the register
        let reg = head regList
        let updatedList = tail regList

        let updatedMap = Map.insert temp reg regMap

        State.put $ ctx { registers = (updatedMap, updatedList) }

        return reg

leave :: Operand.Temporal -> ContextState ()
leave temp = do
    ctx <- State.get

    regMap  <- State.gets (fst . registers)
    regList <- State.gets (snd . registers)

    let register   = fromJust $ Map.lookup temp regMap
    let updatedMap = Map.delete temp regMap
    let updatedSet = register:regList

    State.put $ ctx { registers = (updatedMap, updatedSet) }

getFrame :: ContextState Frame
getFrame = do
    name <- State.gets name
    program <- State.gets program
    let func = fromJust (Map.lookup name program)

    return $ frame func

getFrameFrom :: String -> ContextState Frame
getFrameFrom name = do
    program <- State.gets program
    let func = fromJust (Map.lookup name program)

    return $ frame func

getSymbolPos :: String -> ContextState Int
getSymbolPos name = fromJust . Map.lookup name . vars <$> getFrame