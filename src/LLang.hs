module LLang where

import AST (AST (..), Operator (..))
import Control.Applicative (Alternative (..))
import Combinators
import Expr

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst}
  | ElseIf { cond :: Expr, thn :: LAst}
  | Else { thn :: LAst}
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

parseSpaces :: Parser String String String
parseSpaces = many' (symbol ' ') <|> many' (symbol '\n')

parseComputation :: Parser String String LAst
parseComputation = parseAssigment <|> parseBranching <|> parseCycle

parseBranching :: Parser String String LAst
parseBranching = parseIf <|> parseElseIf <|> parseElse

parseVar :: Parser String String String
parseVar = do 
  symbols "Var"
  parseSpaces
  var <- parseIdent
  parseSpaces
  symbols ";"

parseIf :: Parser String String LAst
parseIf = do 
    symbols "if"
    parseSpaces
    symbols "("
    parseSpaces
    expression <- parseExpr
    parseSpaces
    symbols ")"
    parseSpaces
    symbols "{"
    parseSpaces
    comp <- parseComputation
    parseSpaces
    symbols "}"
    return (If expression comp)

parseElseIf :: Parser String String LAst
parseElseIf = do 
    symbols "else if"
    parseSpaces
    symbols "("
    parseSpaces
    expression <- parseExpr
    parseSpaces
    symbols ")"
    parseSpaces
    symbols "{"
    parseSpaces
    comp <- parseComputation
    parseSpaces
    symbols "}"
    return (ElseIf expression comp)

parseElse :: Parser String String LAst
parseElse = do 
    symbols "else"
    parseSpaces
    symbols "{"
    parseSpaces
    comp <- parseComputation
    parseSpaces
    symbols "}"
    return (Else comp)

parseCycle :: Parser String String LAst
parseCycle = do
    symbols "while"
    parseSpaces
    symbols "("
    parseSpaces
    expression <- parseExpr
    parseSpaces
    symbols ")"
    parseSpaces
    symbols "{"
    parseSpaces
    comp <- parseComputation
    parseSpaces
    symbols "}"
    return (While expression comp)

parseAssigment :: Parser String String LAst
parseAssigment = do
  ident <- parseIdent
  parseSpaces
  symbols "="
  parseSpaces
  expr <- parseExpr
  parseSpaces
  symbols ";"
  parseSpaces
  return (Assign ident expr) 

parseL :: Parser String String LAst
parseL = parseComputation
