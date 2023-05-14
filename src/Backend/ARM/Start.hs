

module Backend.ARM.Start where

import Backend.ARM.Translator (Translator)
import Backend.ARM.Signals (setExit, interrupt)
import Backend.ARM.Function (grantControlTo)
import Backend.ARM.Branches (setCheckpoint)

start :: Translator ()
start = do
    setCheckpoint "_start"
    grantControlTo "main"
    exit

exit :: Translator ()
exit = setExit >> interrupt