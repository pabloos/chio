

{-
    Statement.hs defines how statements (while, if, return ...) are parsed
-}

module Frontend.Parser.Statement where

import Text.Megaparsec

import Frontend.GenericAST
import Frontend.Parser.Parser
import Frontend.Parser.AST
import Frontend.Parser.Type
import Frontend.Parser.Expr

-- all the statement posibilities below (try is using due to overlaping)
statement :: Parser UntypedStatement
statement = try ifElse <|> ifThen <|> while <|> printStmt <|> try call <|> assign <|> var <|> return_

-- print("hola")
printStmt :: Parser UntypedStatement 
printStmt = do
    offset <- getOffset

    reserved "print"
    expr <- parens expr
    return $ Print expr offset

-- int a = 1
var :: Parser UntypedStatement
var = do 
  type' <- typeValue

  offset <- getOffset

  name <- identifier
  reserved "="
  expr <- expr
  return $ Var name type' expr offset

-- a = 2
assign :: Parser UntypedStatement
assign = do
  offset <- getOffset

  name <- identifier
  reserved "="
  expr <- expr
  return $ Assign name expr offset

-- return 
return_ :: Parser UntypedStatement
return_ = do
  offset <- getOffset

  reserved "return"
  expr <- optional expr
  -- optional Lexemes.semicolon
  return $ Return expr offset

{-
    if (a > b) {
        print("a is bigger")
    }
-}
ifThen :: Parser UntypedStatement
ifThen = do
  offset <- getOffset

  reserved "if"
  cond <- parens expr
  then_ <- stmtBlock
  return $ If cond then_ offset


{-
    if (a > b) {
        print("a is bigger")
    } else {
        print("b is bigger")
    }
-}
ifElse :: Parser UntypedStatement
ifElse = do
  offset <- getOffset

  reserved "if"
  cond <- parens expr
  then_ <- stmtBlock
  reserved "else"
  else_ <- stmtBlock
  return $ IfElse cond then_ else_ offset

{-
    while (a > b) {
        print("a is bigger")
    }
-}
while :: Parser UntypedStatement
while = do
  offset <- getOffset

  reserved "while"
  cond <- parens expr
  body <- stmtBlock
  return $ While cond body offset

for :: Parser [UntypedStatement]
for = do
  offset <- getOffset

  reserved "for"
  
  pre   <- reserved "(" >> statement
  cond  <- reserved ";" >> expr
  pos   <- reserved ";" >> statement
  stmts <- reserved ")" >> stmtBlock

  return $ pre : [While cond (stmts ++ [pos]) offset] 

{-
    {
        print("hello")
        return a
    }
-}
stmtBlock :: Parser [UntypedStatement]
stmtBlock = braces $ many statement

{-
    add(1, 2)
-}
call :: Parser UntypedStatement
call = do
  offset <- getOffset

  ident <- identifier
  args <- args
  return $ CallStmt ident args offset