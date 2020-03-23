module Expr where

import AST (AST(..), Operator(..))
import Control.Applicative (Alternative (..))
import Combinators 
import Data.Char (digitToInt, isDigit, isLetter)
import UberExpr


parseNum :: Parser String String Int
parseNum = toNum <$> helper
  where
    toNum = foldl func 0
    func acc '-' = negate acc
    func acc digit   = 10 * acc + digitToInt digit
    helper = do 
      signs  <- many (symbol '-')
      digits <- some (satisfy isDigit)
      return (digits ++ signs)

parseTemplate :: (Monoid e, Read e) => Parser e i AST -> Parser e i Operator -> Parser e i AST
parseTemplate parser operator = do
   (value, values) <- sepBy1' operator parser
   return $ foldl (\res (op, val) -> BinOp op res val) value values

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr [(parseOp2 opMult opDiv, LeftAssoc)] parseTerm BinOp
  where 
    opMult  = symbols "*"
    opDiv   = symbols "/"
  
-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr [(parseOp2 opPlus opMinus, LeftAssoc)] parseMult BinOp
  where 
    opPlus  = symbols "+"
    opMinus = symbols "-"

parseIdent :: Parser String String String
parseIdent = do
  headIdent <- some $ satisfy isLetter <|> symbol '_'
  tailIdent <- many $ satisfy isLetter <|> symbol '_' <|> satisfy isDigit
  return (headIdent ++ tailIdent)

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = uberExpr [
  (parseOp1 opOr,  RightAssoc),
  (parseOp1 opAnd, RightAssoc),
  (parseOpCompare opEqual opNequal opGe opLe opGt opLt, NoAssoc),
  (parseOp2 opPlus opMinus, LeftAssoc), 
  (parseOp2 opMult opDiv,   LeftAssoc),
  (parseOp1 opPow,          RightAssoc)
  ] parseTermOrIdent BinOp
  where 
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

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = Num <$> parseNum <|> (lbr *> parseSum <* rbr)
  where
    lbr = symbol '('
    rbr = symbol ')'

parseTermOrIdent :: Parser String String AST
parseTermOrIdent = Num <$> parseNum <|> Ident <$> parseIdent <|> (lbr *> parseSum <* rbr)
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
                 "&&", "||"]

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
compute (BinOp Plus x y)    = (+) (compute x) (compute y)
compute (BinOp Minus x y)   = (-) (compute x) (compute y)
compute (BinOp Mult x y)    = (*) (compute x) (compute y)
compute (BinOp Div x y)     = div (compute x) (compute y)
compute (BinOp Pow x y)     = (^) (compute x) (compute y)
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
