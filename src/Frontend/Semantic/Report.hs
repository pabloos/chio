

{-
    Report.hs provides some functions to throw errors during the translation process
-}

module Frontend.Semantic.Report where
    
import Control.Monad.State (gets)
import Control.Monad.Except ( MonadError(throwError) )

import           Frontend.Semantic.Analizer (Analizer)
import qualified Frontend.Semantic.Error.Report as Report
import           Frontend.Semantic.Error.Errors ( Error )
import           Frontend.Semantic.State (RecordState(code))

report :: Int -> Error -> Analizer Report.SemanticReport
report offset e = do
    code <- gets code

    let rep = Report.new code offset e

    return rep

reportAt :: Int -> Error -> Analizer ()
reportAt offset e = throwError =<< report offset e