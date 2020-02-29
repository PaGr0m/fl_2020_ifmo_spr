module Expr where

import AST (AST(..), Operator(..))
import Control.Applicative (Alternative (..))
import Combinators 
import Data.Char (digitToInt, isDigit)


-- Парсер чисел
parseNum :: Parser String String Int
parseNum = toNum <$> go 
  where
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = do
      x <- digit
      (x:) <$> (go <|> return [])

parseTemplate :: Alternative f => f AST -> f AST -> f Operator -> f AST
parseTemplate parser1 parser2 operator = 
  (flip BinOp <$> parser1 <*> operator <*> parser2) <|> parser1

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = parseTemplate parseTerm parseMult $ parseOp' opMult opDiv
  where 
    opMult  = symbol '*'
    opDiv   = symbol '/'
  
-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = parseTemplate parseMult parseSum $ parseOp' opPlus opMinus
  where 
    opPlus  = symbol '+'
    opMinus = symbol '-'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = parseSum

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = Num <$> parseNum <|> (lbr *> parseSum <* rbr)
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

parseOp' :: Parser String String Char -> Parser String String Char -> Parser String String Operator
parseOp' op1 op2 = (op1 <|> op2) >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '-'  = return Minus
toOperator '*'  = return Mult
toOperator '+'  = return Plus
toOperator '/'  = return Div
toOperator _    = fail "Failed toOperator"

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Div x y)   = compute x `div` compute y

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing
