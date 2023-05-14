
{- 
    Program.hs defines how a program is parsed
-}

module Frontend.Parser.Program where

import Frontend.Parser.Parser
import Text.Megaparsec

import Frontend.Parser.AST
import Frontend.Parser.Function

program :: Parser UntypedAST
program = sc >> some function