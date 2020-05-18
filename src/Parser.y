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
%%

Rule : LeftPart RightPart           { Rule (LeftPart $1) (RightPart $2) }
LeftPart : nonterminal              { LeftPart (StartTerminal $1) }
RightPart : nonterminal RightPart   { [Nonterminal $1] ++ $2 }
          | terminal RightPart      { [Terminal $1] ++ $2 }
          | nonterminal             { RightPart [Nonterminal $1] }
          | terminal                { RightPart [Terminal $1] }

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
