module Grammar where

import Text.Printf (printf)


data Grammar term nonterm = Grammar
  { terms        :: [term]
  , nonterms     :: [nonterm]
  , rules        :: [Rules term nonterm]
  , startNonterm :: nonterm
  } deriving (Eq, Show)
  
data Rule = Rule LeftPart RightPart
          deriving Show

data LeftPart = Nonterminal String
              deriving Show

type RightPart = [Symbol]

data Symbol = Terminal String 
            | Nonterminal String
            deriving Show