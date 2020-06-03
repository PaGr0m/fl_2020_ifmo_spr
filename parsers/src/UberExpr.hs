module UberExpr where

import AST
import Combinators
import Debug.Trace
import Control.Applicative

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: (Monoid e, Read e)
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> Parser e i ast
uberExpr [] parser _ = parser       
uberExpr ((operator, asc) : asts) parser binaryConstructor =
    let 
        unaryUber = uberExpr asts parser binaryConstructor  
    in
    case asc of 
        Binary assoc -> case assoc of
            LeftAssoc  -> left (sepBy1' operator (uberExpr asts parser binaryConstructor)) <|> unaryUber
            RightAssoc -> right (sepBy1'' operator (uberExpr asts parser binaryConstructor)) <|> unaryUber
            NoAssoc    -> noAsc (uberExpr asts parser binaryConstructor) <|> unaryUber
    where 
        right val = val >>= \(x, xs) -> 
            return $ foldr (flip $ \ast1 (op, ast2) -> binaryConstructor op ast2 ast1) x xs
        left val = val >>= \(x, xs) -> 
            return $ foldl (\ast1 (op, ast2)        -> binaryConstructor op ast1 ast2) x xs
        noAsc parser = do 
            ast1 <- parser 
            op <- operator
            ast2 <- parser
            return $ binaryConstructor op ast1 ast2
