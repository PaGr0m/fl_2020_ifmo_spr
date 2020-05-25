{ -- happy
module Parser where

import Data.Char
import qualified Lexer
}

%name parse
%tokentype { Lexer.Token }
%error { parseError }

%token
    terminal    { Lexer.TokenTerminal _ }
    nonterminal { Lexer.TokenNonterminal _ }
    epsilon     { Lexer.TokenEpsilon _ }
    delimiter   { Lexer.TokenDelimiter _ }
%%

Rules : Rule Rules                                  { ($1:$2) }
      | Rule                                        { [$1] }
Rule : LeftPart RightPart delimiter                 { Rule $1 $2 }
LeftPart : nonterminal                              { StartTerminal (Lexer.tokenValue $1) }
RightPart : epsilon                                 { [] }
          | ActualRightPart                         { $1 }
ActualRightPart : nonterminal ActualRightPart       { [Nonterminal (Lexer.tokenValue $1)] ++ $2 }
                | terminal ActualRightPart          { [Terminal (Lexer.tokenValue $1)] ++ $2 }
                | nonterminal                       { [Nonterminal (Lexer.tokenValue $1)] }
                | terminal                          { [Terminal (Lexer.tokenValue $1)] }

{
data Rule = Rule LeftPart RightPart
            deriving Show
data LeftPart = StartTerminal String
                deriving Show

type RightPart = [Symbol]

data Symbol = Terminal String
            | Nonterminal String
            deriving Show

parseError :: [Lexer.Token] -> a
parseError ts = error ("Parse error")
}
