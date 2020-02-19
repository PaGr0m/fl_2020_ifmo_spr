module Arith where

import           Data.Char   (digitToInt, isDigit)
import qualified Sum         (parseNum, splitOn)
import           Text.Printf (printf)

data Operator
  = Plus
  | Mult
  | Minus
  | Div
  deriving (Eq)

data AST
  = BinOp Operator AST AST
  | Num Int
  deriving (Eq)

parseExpr :: String -> Maybe (AST, String)
parseExpr = parseSum

parseMult :: String -> Maybe (AST, String)
parseMult input = do
  (x, input') <- parseNum input
  case parseOp input' of
    Just (op, input'')
      | op == Mult || op == Div -> do
        (y, input''') <- parseMult input''
        return (BinOp op (Num x) y, input''')
    _ -> return (Num x, input')

parseSum :: String -> Maybe (AST, String)
parseSum input = do
  (x, input') <- parseMult input
  case parseOp input' of
    Just (op, input'')
      | op == Plus || op == Minus -> do
        (y, input''') <- parseSum input''
        return (BinOp op x y, input''')
    _ -> return (x, input')

parseNum :: String -> Maybe (Int, String)
parseNum input =
  let (acc, rest) = span isDigit input
   in if null acc
        then Nothing
        else Just (Sum.parseNum acc, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('-':input) = Just (Minus, input)
parseOp ('+':input) = Just (Plus, input)
parseOp ('*':input) = Just (Mult, input)
parseOp ('/':input) = Just (Div, input)
parseOp _           = Nothing

evaluate :: String -> Maybe Int
evaluate input = do
  (ast, rest) <- parseExpr input
  return $ compute ast

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Div x y)   = compute x `div` compute y

instance Show Operator where
  show Plus  = "+"
  show Mult  = "*"
  show Minus = "-"
  show Div   = "/"

instance Show AST where
  show = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0
           then printf "%s|_%s" (concat $ replicate (n - 1) "| ")
           else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+ 1)
