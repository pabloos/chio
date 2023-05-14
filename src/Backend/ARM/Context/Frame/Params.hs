

{-
    Params.hs defines methods to manage params from the stack frame
-}

module Backend.ARM.Context.Frame.Params where

import           Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Control.Monad.State as State

import Frontend.GenericAST (Param)
import Backend.ARM.Context.Context     (ContextState, Context (program), FunctionState (params))
import Backend.ARM.Context.Frame.Frame (Parameter, Frame (parameters))
import Backend.ARM.Context.Allocator   (getFrameFrom, getFrame)

getParamIndexFrom :: String -> String -> ContextState Parameter
getParamIndexFrom funcName symbolName = do
    frame <- getFrameFrom funcName
    return $ fromJust $ Map.lookup symbolName $ parameters frame

getParamsOf :: String -> ContextState [Param]
getParamsOf name = do
    program <- State.gets program
    return $ params $ fromJust (Map.lookup name program)

getParamPos :: String -> ContextState Parameter
getParamPos name = fromJust . Map.lookup name . parameters <$> getFrame