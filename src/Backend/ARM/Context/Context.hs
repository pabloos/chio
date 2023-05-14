

{-
    Context.hs describe the context that a IR function translation needs
-}

module Backend.ARM.Context.Context where

import           Control.Monad.State ( State, gets, modify )
import qualified Data.Map as Map

import           Frontend.GenericAST (Param)
import qualified Backend.IR.Spec.Instructions as IR
import           Backend.ARM.Context.Registers (RegistersVault, newRegSet)
import           Backend.ARM.Context.Frame.Frame (Frame)
import           Backend.ARM.Context.Frame.Build (buildFrame)
import           Backend.ARM.Spec.DataSection (DataSection, emptyData)

type ContextState = State Context

data Context = Context {
    name      :: String,
    program   :: ProgramState,
    registers :: RegistersVault,
    datasection   :: DataSection,
    counter   :: Int
}

newCtx :: [IR.Function] -> Context
newCtx funcs = Context{ 
        name = "", 
        program = buildProgramState funcs, 
        registers = newRegSet,
        datasection = emptyData, -- Map.empty,
        counter = 0
    }

data FunctionState = FunctionState {
    frame :: Frame,
    params :: [Param]
}

type ProgramState = Map.Map String FunctionState

buildProgramState :: [IR.Function] -> ProgramState
buildProgramState (func@(_, name, params, stmts):xs) = do
    let frame = buildFrame func

    let state = Map.singleton name FunctionState{ frame = frame, params = params }

    state `Map.union` buildProgramState xs

buildProgramState [] = Map.empty

getName :: ContextState String
getName = gets name

setName :: String -> ContextState ()
setName func = modify (\ctx -> ctx {name = func})

-- counter
useCounter :: ContextState Int
useCounter = do
    i <- gets counter
    modify (\c -> c{counter = i+1})

    return i