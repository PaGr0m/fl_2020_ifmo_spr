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
    

unit_parseElseIf :: Assertion
unit_parseElseIf = do
    runParser parseElseIf "else if (42>2) {x=2}" @?= Success "" 
        (ElseIf 
            ((BinOp Gt (Num 42) (Num 2))) 
             (Assign "x" (Num 2))
            )

unit_parseCycle :: Assertion
unit_parseCycle = do
    runParser parseCycle "while (3==3) {x=2}" @?= Success "" 
        (While 
            (BinOp Equal (Num 3) (Num 3)) 
             (Assign "x" (Num 2))
            )

unit_parseAssigment :: Assertion
unit_parseAssigment = do
    runParser parseAssigment "abc=239;" @?= Success "" 
        (Assign "abc" (Num 239))
