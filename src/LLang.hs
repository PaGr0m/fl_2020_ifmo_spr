module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..))
import Control.Monad
import Expr
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { 
  subst :: Subst,
  input :: [Int], 
  output :: [Int] 
} deriving (Show, Eq)


data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

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
parseL = error "parseL undefined"

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

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
