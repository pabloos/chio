

{-
    Expr.hs contains all the parser of expressions
    with the precedence table
-}

module Frontend.Parser.Expr where

import Control.Monad.Combinators.Expr

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Operations
import Frontend.GenericAST (Literal(..))
import Frontend.Parser.Parser (Parser, parens, identifier, sc, symbol, natural, quotes, reserved, comma)
import Frontend.Parser.AST (UntypedExpr (Lit, Arith, Logic, Comp, SymbolRef, Negate, Not, Call))

expr :: Parser UntypedExpr
expr = makeExprParser term operatorTable

term :: Parser UntypedExpr
term = choice [try call_, parens expr, int, bool, string, ident]

-- operator precedence defined with a table
operatorTable :: [[Operator Parser UntypedExpr]]
operatorTable =
  [ [ Prefix (do
        pos <- getOffset
        symbol "-"
        return $ Negate pos)
     , Prefix (do
        pos <- getOffset
        symbol "!"
        return $ Not pos)
    ]
  , [ InfixL (do
        pos <- getOffset
        symbol "*"
        return $ Arith Mul pos)

     , InfixL (do
        pos <- getOffset
        symbol "&&"
        return $ Logic And pos)
    ]
  , [ InfixL (do
        pos <- getOffset
        symbol "+"
        return $ Arith Add pos)
    , InfixL (do
        pos <- getOffset
        symbol "-"
        return $ Arith Sub pos)
    , InfixL (do
        pos <- getOffset
        symbol "||"
        return $ Logic Or pos)
    ]
  , [ InfixL (do
        pos <- getOffset
        symbol ">"
        return $ Comp Gt pos)
    , InfixL (do
        pos <- getOffset
        symbol "<"
        return $ Comp Lt pos)
    , InfixL (do
        pos <- getOffset
        symbol "*"
        return $ Comp Ge pos)
    , InfixL (do
        pos <- getOffset
        symbol "*"
        return $ Comp Le pos)
    ]
  , [ InfixL (do
        pos <- getOffset
        symbol "=="
        return $ Comp Eq pos)
    , InfixL (do
        pos <- getOffset
        symbol "*"
        return $ Comp Ne pos)
    ]
  ]

ident :: Parser UntypedExpr
ident = do
    pos <- getOffset
    SymbolRef pos <$> identifier

call_ :: Parser UntypedExpr
call_ = do
  pos <- getOffset
  ident <- identifier
  Call pos ident <$> args

args :: Parser [UntypedExpr]
args = parens $ expr `sepBy` comma

-- Literals
bool :: Parser UntypedExpr
bool = do
    lit <- True <$ reserved "true" <|> False <$ reserved "false"
    Lit (BoolVal lit) <$> getOffset

int :: Parser UntypedExpr
int = do
    lit <- L.signed sc natural
    Lit (IntVal lit) <$> getOffset

string :: Parser UntypedExpr
string = do
    lit <- quotes $ many (noneOf "\"")
    Lit (StringVal lit) <$> getOffset