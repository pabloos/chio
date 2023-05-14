

{-
    Function.hs defines how a function is parsed
-}

{-# LANGUAGE NamedFieldPuns #-}

module Frontend.Parser.Function where

import Text.Megaparsec

import Frontend.GenericAST hiding (params)
import Frontend.Parser.Parser
import Frontend.Parser.AST
import Frontend.Parser.Statement
import Frontend.Parser.Type

{-
    function parses a function head and body

    example:

    fun add(a int, b int) -> int {
        return a + b
    }
-}
function :: Parser UntypedFunction
function = do
    offset <- getOffset

    reserved "fun"
    name <- identifier
    params <- params
    returnType <- (reserved "->" >> retSig) <|> noRetSig
    stmts <- stmtBlock

    return $ Function name params returnType stmts offset 

-- params parses '(param1 int, param2 bool)'
params :: Parser [Param]
params = parens $ sepBy param comma

param :: Parser Param
param = do
    parsePos <- getOffset

    name <- identifier
    type_ <- typeValue

    return Param { name, type_, parsePos }

-- retSig parses '-> int'
retSig :: Parser ReturnSignature
retSig = do
    pos <- getOffset
    type_ <- typeValue
    return $ ReturnSignature { pos = pos, typeSignature = Just type_ }

-- no return signature (a procedure)
noRetSig :: Parser ReturnSignature
noRetSig = do
    pos <- getOffset
    return $ ReturnSignature { pos = pos, typeSignature = Nothing }