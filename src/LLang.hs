module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators
import Control.Monad
import Control.Applicative
import Expr 
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
  deriving (Eq)

parseSpaces :: Parser String String String
parseSpaces = many (symbol ' ') <|> many (symbol '\n')

parseComputation :: Parser String String LAst
parseComputation = parseAssigment <|> parseBranching <|> parseCycle

parseBranching :: Parser String String LAst
parseBranching = parseIf *> parseElseIf *> parseElse

parseVar :: Parser String String String
parseVar = do 
    var <- parseIdent
    parseSpaces
    symbols ";"
    return var

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
    return If {
        cond = expression,
        thn = comp
    } 

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
    return If {
        cond = expression,
        thn = comp
    } 

parseElse :: Parser String String LAst
parseElse = do 
    symbols "else"
    parseSpaces
    symbols "{"
    parseSpaces
    comp <- parseComputation
    parseSpaces
    symbols "}"
    return If {
        cond = Num 0,
        thn = comp
        -- els = Num 0
    }

parseRead :: Parser String String LAst
parseRead = do 
    symbols "read"
    parseSpaces
    var <- parseVar
    return (Read var) 

parseWrite :: Parser String String LAst
parseWrite = do
    symbols "write"
    parseSpaces
    expr <- parseExpr
    return (Write expr) 

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

parseReturn :: Parser String String LAst
parseReturn = do
    symbols "return"
    parseSpaces
    expr <- parseExpr
    parseSpaces
    symbols ";"
    return (Return expr)

parseArgs :: Parser String String [String]
parseArgs = (fmap (:) parseIdent <*> many (parseSpaces *> symbols "," *> parseSpaces *> parseIdent)) <|> pure []
 
parseFuncs :: Parser String String [Function]
parseFuncs = (fmap (:) parseDef <*> many (parseSpaces *> parseDef)) <|> pure []

parseDef :: Parser String String Function
parseDef = do
  symbols "fun"
  parseSpaces
  funName <- parseIdent
  parseSpaces
  symbols "("
  parseSpaces
  funArgs <- parseArgs
  parseSpaces
  symbols ")"
  parseSpaces
  symbols "{"
  parseSpaces
  funBody <- parseComputation
  symbols "}"
  parseSpaces
  return $ Function funName funArgs funBody

parseProg :: Parser String String Program
parseProg = do
  functions <- parseFuncs
  prog <- parseL 
  return $ Program functions prog

parseFunctionCall :: Parser String String AST
parseFunctionCall = do
  return $ FunctionCall"" []

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseL :: Parser String String LAst
parseL = parseComputation <|> parseRead <|> parseWrite

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) conf = do
  value <- evalExpr (subst conf) cond
  let branch = if value /= 0 then thn else els
  eval branch conf

eval (While cond body) conf = do
  value <- evalExpr (subst conf) cond
  if value == 0 
    then return conf 
    else do 
      conf' <- eval body conf
      eval (While cond body) conf'

eval (Assign var expr) conf = do
  value <- evalExpr (subst conf) expr
  return Conf {
    subst  = Map.insert var value (subst conf), 
    input  = input conf, 
    output = output conf
  }

eval (Read var) conf = do
  let inp = input conf
  guard (not $ null inp)
  let (value : input') = inp
  return Conf {
    subst = Map.insert var value (subst conf),
    input = input',
    output = output conf
  }

eval (Write expr) conf = do
  value <- evalExpr (subst conf) expr
  return Conf {
    subst = subst conf,
    input = input conf,
    output = value : (output conf)
  }

eval (Seq []) conf = return conf
eval (Seq (lAst:lAsts)) conf = do
  conf' <- eval lAst conf
  eval (Seq lAsts) conf'

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
