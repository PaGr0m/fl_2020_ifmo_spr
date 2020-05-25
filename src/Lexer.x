{
module Lexer where
}

%wrapper "basic"

$terminal = [a-z]
$nonterminal = [A-Z]
$epsilon = [\#]
$delim = [\;]

tokens :-

    $white+ ;
    $terminal+      {TokenTerminal}
    $nonterminal+   {TokenNonterminal}
    $epsilon        {TokenEpsilon}
    $delim          {TokenDelimiter}

{
data Token = TokenTerminal String
           | TokenNonterminal String
           | TokenEpsilon String
           | TokenEmpty
           | TokenDelimiter String
           deriving Show

tokenValue (TokenTerminal v) = v
tokenValue (TokenNonterminal v) = v
tokenValue (TokenEpsilon v) = v
tokenValue (TokenDelimiter v) = v
tokenValue _ = undefined

}
