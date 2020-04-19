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
    runParser (parseIf *> parseElse) "if (x) {x = 13;}else {x = 42;}"  @?= 
        Success "" (If {cond = Num 0, thn = Assign {var = "x", expr = Num 42}})
    runParser (parseIf) "if (x) {x = 13;}" @?= 
        Success "" (If {cond = Ident "x", thn = Assign {var = "x", expr = Num 13}})
    runParser (parseIf *> parseElse) "if (x) {x = 13;} else {x = 42;}" @?= 
        Failure "Predicate failed"

unit_parseCycle :: Assertion
unit_parseCycle = do
    runParser parseCycle "while (3==3) {x=2;}" @?= 
        Success "" (While (BinOp Equal (Num 3) (Num 3)) (Assign "x" (Num 2)))
    runParser parseCycle "while (x) {x=42;}" @?= 
        Success "" (While (Ident "x") (Assign "x" (Num 42)))

unit_parseAssigment :: Assertion
unit_parseAssigment = do
    runParser parseAssigment "abc=239;" @?= 
        Success "" (Assign {var = "abc", expr = Num 239})
    runParser parseAssigment "counter=0;" @?= 
        Success "" (Assign {var = "counter", expr = Num 0})
    runParser parseAssigment "abc=239" @?= 
        Failure "Predicate failed"

unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "read x;" @?= Success "" (Read {var = "x"})

unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "write 42" @?= Success "" (Write (Num 42))
    