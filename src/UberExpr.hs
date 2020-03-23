    module UberExpr where

import AST
-- import Expr
import Combinators
import Debug.Trace
import Control.Applicative

data Associativity = LeftAssoc | RightAssoc | NoAssoc
    deriving Show

uberExpr :: (Monoid e, Read e)
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast
uberExpr [] parser _ = parser       
uberExpr ((operator, asc) : asts) parser constructor =
    case asc of
        RightAssoc -> right $ sepBy1'' operator (uberExpr asts parser constructor)
        LeftAssoc  -> left  $ sepBy1'  operator (uberExpr asts parser constructor)
        NoAssoc    -> noAsc (uberExpr asts parser constructor)
    where 
        right val = val >>= \(x, xs) -> 
            return $ foldr (flip $ \ast1 (op, ast2) -> constructor op ast2 ast1) x xs
        left val = val >>= \(x, xs) -> 
            return $ foldl (\ast1 (op, ast2)        -> constructor op ast1 ast2) x xs
        noAsc parser = (do 
            ast1 <- parser 
            op <- operator
            ast2 <- parser
            return $ constructor op ast1 ast2 ) <|> parser
