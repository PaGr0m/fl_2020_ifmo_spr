module Expr where

import AST (AST (..), Operator (..))
import Control.Applicative (Alternative (..))
import Combinators
import UberExpr
import Data.Char
import Data.Bool


-- Оптимизатор выражения
optimize :: AST -> AST
optimize (BinOp op (Num x) (Num y)) = Num $ applyOp op x y

optimize ast@(BinOp op (Num x) (Ident y))
  | op == Plus && x == 0  = Ident y
  | op == Mult && x == 0  = Num 0
  | op == Mult && x == 1  = Ident y
  | otherwise             = ast

optimize ast@(BinOp op (Ident x) (Num y))
  | op == Plus && y == 0  = Ident x
  | op == Mult && y == 0  = Num 0
  | op == Mult && y == 1  = Ident x
  | otherwise             = ast

optimize (BinOp oop (BinOp iop i@(Ident _) (Num l)) (Num r))
    | oop == iop = optimize (BinOp oop i (Num $ applyOp oop l r))

optimize (BinOp oop (BinOp iop (Num l) i@(Ident _)) (Num r))
    | oop == iop = optimize (BinOp oop i (Num $ applyOp oop l r))

optimize (BinOp op left right) = 
    case BinOp op (optimize left) (optimize right) of
        ast@(BinOp op (Num _)   (Num _))    -> optimize ast
        ast@(BinOp op (Ident _) (Num _))    -> optimize ast
        ast@(BinOp op (Num _)   (Ident _))  -> optimize ast
        ast@(BinOp op (Ident _) (Ident _))  -> optimize ast
        ast                                 -> ast
        
optimize ast = ast

-- Парсер арифметических выражений над целыми числами
parseExpr :: Parser String String AST
parseExpr = uberExpr [
  (parseOp1 opPlus, Binary LeftAssoc), 
  (parseOp1 opMult, Binary LeftAssoc)
  ] parseTermOrIdent BinOp
  where 
    opPlus    = symbols "+"
    opMult    = symbols "*"

parseIdent :: Parser String String String
parseIdent = do
  headIdent <- some $ satisfy isLetter <|> symbol '_'
  tailIdent <- many $ satisfy isLetter <|> symbol '_' <|> satisfy isDigit
  return (headIdent ++ tailIdent)

-- Парсер чисел
parsePosNum :: Parser String String Int
parsePosNum = toNum <$> go 
  where
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = do
      x <- digit
      (x:) <$> (go <|> return [])

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

-- Парсер для операторов
-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTermOrIdent :: Parser String String AST
parseTermOrIdent = Num <$> parsePosNum <|> Ident <$> parseIdent <|> (lbr *> parseExpr <* rbr)
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсеры для операторов
parseOp :: Parser String String Operator
parseOp = go operators >>= toOperator
  where
    go [x]    = symbols x
    go (x:xs) = symbols x <|> go xs
    operators = ["+", "*"]

parseOp1 :: Parser String String String -> Parser String String Operator
parseOp1 op = op >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = return Plus
toOperator "*"  = return Mult
toOperator _    = fail' "Failed toOperator"

applyOp :: Operator -> Int -> Int -> Int
applyOp op left right = compute (BinOp op (Num left) (Num right))

compute :: AST -> Int
compute (Num x)             = x
compute (BinOp Plus x y)    = (+) (compute x) (compute y)
compute (BinOp Mult x y)    = (*) (compute x) (compute y)

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing
