{
module Lexer where
}

%wrapper "basic"

$terminal = [a-z]
$nonterminal = [A-Z]

tokens :-

    $white+ ;
    $terminal+      {TokenTerminal}
    $nonterminal+   {TokenNonterminal}

{
data Token = TokenTerminal String 
           | TokenNonterminal String
           | TokenEmpty
           deriving Show
}
