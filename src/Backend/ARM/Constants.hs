

{-
    Constants.hs provides a function to store constant strings in the data section
    during the translation process
-}

module Backend.ARM.Constants where

import qualified Data.Map as Map
import           Control.Monad.Writer (MonadTrans(lift))
import           Control.Monad.State (gets, modify)

import           Backend.ARM.Translator (Translator)
import           Backend.ARM.Context.Context ( Context(datasection, counter), useCounter )
import           Backend.ARM.Spec.DataSection
                    (newConstStr, DataSection (strings, DataSection), AsciiText (strLabel))

storeStr :: String -> Translator String
storeStr str = do
    index <- lift useCounter
    dat <- gets datasection

    let ascii = newConstStr str index

    let strs = strings dat ++ [ascii]

    modify (\ctx -> ctx{datasection = DataSection { strings = strs } })

    return (strLabel ascii)