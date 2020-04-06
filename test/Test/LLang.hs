module Test.LLang where

import AST                
import Combinators
import Control.Applicative ((<|>))
import Expr                

import Test.Tasty.HUnit    (Assertion (..), (@?=))
import Text.Printf         (printf)
import LLang


unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "if(42>2){x=2}" @?= Success "" 
        (If 
            ((BinOp Gt (Num 42) (Num 2))) 
            (Assign "x" (Num 2))
            )
    runParser parseIf "if(x<2){y=2}" @?= Success "" 
        (If 
            ((BinOp Gt (Ident "x") (Num 2))) 
            (Assign "y" (Num 2))
            )

unit_parseElse :: Assertion
unit_parseElse = do
    runParser parseElse "else {x=2}" @?= Success "" 
        (Else (Assign "x" (Num 2)))
    