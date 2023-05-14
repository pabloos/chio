

{-
    Main.hs contains the main function to call from the CLI
-}

module Main where

import System.Environment (getArgs)
import Run (runCompiler)

main :: IO()
main = do
  (source:dest:_) <- getArgs

  code <- readFile source

  asm <- runCompiler code

  writeFile dest asm