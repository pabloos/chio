

{-
    Run.hs contains the way the programs assemble the frontend with the backend
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Run (runCompiler) where

import qualified Frontend.Frontend as Frontend
import           Frontend.Semantic.AST (TypedAST)
import qualified Backend.Backend as Backend
import           Backend.ARM.Spec.Program (Program)

runCompiler :: String -> IO String
runCompiler src = do
    let front :: (Either String TypedAST) = Frontend.compile src
    case front of
      Left e -> putStr e >> return ""
      Right subs -> do
        let program :: Program = Backend.compile subs
        return $ show program