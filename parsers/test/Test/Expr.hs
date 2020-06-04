module Test.Expr where

import           AST              (AST (..), Operator (..))
import           Combinators      (Result (..), runParser)
import           Expr             (evaluate, parseNum, parseOp,
                                   parseExpr, parseIdent, optimize)
import           Test.Tasty.HUnit (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_evaluate :: Assertion
unit_evaluate = do
    evaluate "1" @?= Just 1
    evaluate "1+2" @?= Just (1+2)
    evaluate "2+4+8" @?= Just (2+4+8)
    evaluate "11+22" @?= Just (11+22)
    evaluate "13+42+777" @?= Just (13+42+777)
    evaluate "31+24+777" @?= Just (31+24+777)
    evaluate "1+2*3+4" @?= Just (1+2*3+4)
    evaluate "12+23*34+456" @?= Just (12+23*34+456)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
    isFailure (runParser parseNum "+3") @?= True
    isFailure (runParser parseNum "a") @?= True

unit_parseIdent :: Assertion
unit_parseIdent = do
    runParser parseIdent "abc def" @?= Success " def" "abc"
    runParser parseIdent "AbC dEf" @?= Success " dEf" "AbC"
    runParser parseIdent "_123" @?= Success "" "_123"
    runParser parseIdent "a_b_c d_e" @?= Success " d_e" "a_b_c"
    runParser parseIdent "x_ " @?= Success " " "x_"
    runParser parseIdent "abc123" @?= Success "" "abc123"
    runParser parseIdent "_" @?= Success "" "_"
    runParser parseIdent "abc*1" @?= Success "*1" "abc"
    assertBool "" $ isFailure $ runParser parseIdent "123abc"
    assertBool "" $ isFailure $ runParser parseIdent "123"
    assertBool "" $ isFailure $ runParser parseIdent ""


unit_parseOp :: Assertion
unit_parseOp = do
    runParser parseOp "+1" @?= Success "1" Plus
    runParser parseOp "**" @?= Success "*" Mult
    isFailure (runParser parseOp "12") @?= True

unit_parseExpr :: Assertion
unit_parseExpr = do
    runParser parseExpr "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseExpr "123"     @?= Success "" (Num 123)
    runParser parseExpr "abc"     @?= Success "" (Ident "abc")
    runParser parseExpr "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseExpr "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    runParser parseExpr "1*x*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    runParser parseExpr "xyz"     @?= Success "" (Ident "xyz")
    runParser parseExpr "1*x+z*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    runParser parseExpr "1+y*3+z" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    runParser parseExpr "1+x" @?= Success "" (BinOp Plus (Num 1) (Ident "x"))


unit_optimize :: Assertion
unit_optimize = do
    {-
           Plus
          /    \
        Mult   Num 3        -->         Num 5
       /    \
    Num 1   Num 2
    -}
    optimize (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (Num 3)) @?= (Num 5)

    {-
           Mult
          /    \
        Mult   Num 3        -->         Num 6
       /    \
    Num 1   Num 2
    -}
    optimize (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3)) @?= (Num 6)

    {-
           Mult
          /    \
        Plus   Num 3        -->         Num 9
       /    \
    Num 1   Num 2
    -}
    optimize (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) @?= (Num 9)

    {-
           Mult                             Mult
          /    \                          /      \
        Plus   Num 3        -->         Plus   Num 3
       /    \                          /    \
    Num 5   Ident x                 Num 5   Ident x
    -}
    optimize (BinOp Mult (BinOp Plus (Num 5) (Ident "x")) (Num 3)) @?= 
        (BinOp Mult (BinOp Plus (Num 5) (Ident "x")) (Num 3))

    {-
           Mult                             Mult
          /    \                          /      \
        Mult   Num 3        -->       Ident x   Num 3
       /    \                          
    Num 1   Ident x                 
    -}
    optimize (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3)) @?= 
        (BinOp Mult (Ident "x") (Num 3))

    {-
           Mult                            
          /    \                         
        Mult   Num 3        -->         Num 0
       /    \                          
    Num 0   Ident x                 
    -}
    optimize (BinOp Mult (BinOp Mult (Num 0) (Ident "x")) (Num 3)) @?= (Num 0)

    {-
           Plus                              Plus         
          /    \                           /      \
        Mult   Ident x       -->        Num 25    Ident x
       /    \                          
    Num 5   Num 5                 
    -}
    optimize (BinOp Plus (BinOp Mult (Num 5) (Num 5)) (Ident "x")) @?= (BinOp Plus (Num 25) (Ident "x"))

    {-
           Mult                                 
          /    \                           
        Mult   Ident x       -->        Num 0
       /    \                          
    Num 5   Num 5                 
    -}
    optimize (BinOp Mult (BinOp Mult (Num 0) (Num 5)) (Ident "x")) @?= (Num 0)

    runParser (optimize <$> parseExpr) "0+1" @?= Success "" (Num 1)
    runParser (optimize <$> parseExpr) "0+1*x" @?= Success "" (Ident "x")
