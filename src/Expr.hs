module Expr where

import AST (AST (..), Operator (..), Subst (..))
import Control.Applicative (Alternative (..))
import Combinators
import UberExpr
import Data.Char
import Data.Bool
import qualified Data.Map as Map


evalExpr :: Subst -> AST -> Maybe Int
evalExpr subst (Num num) = Just num
evalExpr subst (Ident ident) = Map.lookup ident subst
evalExpr subst (UnaryOp op ast) = do 
  value <- evalExpr subst ast
  let ast' = UnaryOp op (Num value)
  return $ compute ast'
evalExpr subst (BinOp op ast1 ast2) = do 
  value1 <- evalExpr subst ast1
  value2 <- evalExpr subst ast2
  let ast' = BinOp op (Num value1) (Num value2) 
  return $ compute ast'

-- Парсер арифметических выражений над целыми числами
parseExpr :: Parser String String AST
parseExpr = uberExpr [
  (parseOp1 opOr,  Binary RightAssoc),
  (parseOp1 opAnd, Binary RightAssoc),
  (parseOp1 opNot, Unary),
  (parseOpCompare opEqual opNequal opGe opLe opGt opLt, Binary NoAssoc),
  (parseOp2 opPlus opMinus, Binary LeftAssoc), 
  (parseOp2 opMult opDiv,   Binary LeftAssoc),
  (parseOp1 opMinus, Unary),
  (parseOp1 opPow, Binary RightAssoc)
  ] parseTermOrIdent BinOp UnaryOp
  where 
    opNot     = symbols "!"
    opOr      = symbols "||"
    opAnd     = symbols "&&"
    opEqual   = symbols "=="
    opNequal  = symbols "/="
    opGe      = symbols ">="
    opLe      = symbols "<="
    opGt      = symbols ">"
    opLt      = symbols "<"
    opPlus    = symbols "+"
    opMinus   = symbols "-"
    opMult    = symbols "*"
    opDiv     = symbols "/"
    opPow     = symbols "^"

parseIdent :: Parser String String String
parseIdent = do
  headIdent <- some $ satisfy isLetter <|> symbol '_'
  tailIdent <- many $ satisfy isLetter <|> symbol '_' <|> satisfy isDigit
  return (headIdent ++ tailIdent)

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = toNum <$> go 
  where
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = do
      x <- digit
      (x:) <$> (go <|> return [])

-- Парсер для операторов
-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTermOrIdent :: Parser String String AST
parseTermOrIdent = Num <$> parseNum <|> Ident <$> parseIdent <|> (lbr *> parseExpr <* rbr)
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсеры для операторов
parseOp :: Parser String String Operator
parseOp = go operators >>= toOperator
  where
    go [x]    = symbols x
    go (x:xs) = symbols x <|> go xs
    operators = ["+", "-", "*", "/", "^", 
                 "==", "/=", "<=", ">=", ">", "<", 
                 "&&", "||", "!"]

parseOp1 :: Parser String String String -> Parser String String Operator
parseOp1 op = op >>= toOperator

parseOp2 :: Parser String String String -> Parser String String String -> Parser String String Operator
parseOp2 op1 op2 = (op1 <|> op2) >>= toOperator

parseOpCompare :: Parser String String String
  -> Parser String String String
  -> Parser String String String
  -> Parser String String String
  -> Parser String String String
  -> Parser String String String
  -> Parser String String Operator
parseOpCompare op1 op2 op3 op4 op5 op6 = (op1 <|> op2 <|> op3 <|> op4 <|> op5 <|> op6) >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "!"  = return Not
toOperator "||" = return Or
toOperator "&&" = return And
toOperator "==" = return Equal
toOperator "/=" = return Nequal
toOperator ">"  = return Gt
toOperator ">=" = return Ge
toOperator "<"  = return Lt
toOperator "<=" = return Le
toOperator "+"  = return Plus
toOperator "-"  = return Minus
toOperator "*"  = return Mult
toOperator "/"  = return Div
toOperator "^"  = return Pow
toOperator _    = fail "Failed toOperator"

compute :: AST -> Int
compute (Num x)             = x
compute (UnaryOp Minus x)   = - (compute x)
compute (BinOp Plus x y)    = (+) (compute x) (compute y)
compute (BinOp Minus x y)   = (-) (compute x) (compute y)
compute (BinOp Mult x y)    = (*) (compute x) (compute y)
compute (BinOp Div x y)     = div (compute x) (compute y)
compute (BinOp Pow x y)     = (^) (compute x) (compute y)
compute (UnaryOp Not x)     = fromEnum $ (==) (compute x) 0
compute (BinOp Equal x y)   = fromEnum $ (==) (compute x) (compute y)
compute (BinOp Nequal x y)  = fromEnum $ (/=) (compute x) (compute y)
compute (BinOp Gt x y)      = fromEnum $ (>)  (compute x) (compute y)
compute (BinOp Ge x y)      = fromEnum $ (>=) (compute x) (compute y)
compute (BinOp Lt x y)      = fromEnum $ (<)  (compute x) (compute y)
compute (BinOp Le x y)      = fromEnum $ (<=) (compute x) (compute y)
compute (BinOp Or x y)      = fromEnum $ (||) (intToBool (compute x)) (intToBool (compute y))
compute (BinOp And x y)     = fromEnum $ (&&) (intToBool (compute x)) (intToBool (compute y))

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing
