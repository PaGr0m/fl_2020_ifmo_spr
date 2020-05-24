module Converter where

import Parser
import Grammar
import qualified Data.List
import qualified Data.Map as Map


getStartTerminal (Rule (StartTerminal term) _) = term

updateMap term sym map 
    | Map.member term map   = Map.adjust (++ [Rhs $ fmap convertSymbol sym]) term map
    | otherwise             = Map.insert term [Rhs $ fmap convertSymbol sym] map

termPredicate (Terminal _) = True
termPredicate _            = False

convertSymbol (Terminal    sym) = Term sym
convertSymbol (Nonterminal sym) = Nonterm sym

findNonterminals symbols = Data.List.nub $ fmap (\(Rule (StartTerminal sym) _) -> sym) symbols
findTerminals symbols = Data.List.nub $ fmap (\(Terminal sym) -> sym) (filter termPredicate (concatMap (\ (Rule _ term) -> term) symbols))

findRules = foldr (\(Rule (StartTerminal term) sym) map -> updateMap term sym map) Map.empty 


grammarConverter :: [Rule] -> Grammar String String
grammarConverter grammar = Grammar {
        terms           = findTerminals grammar,
        nonterms        = findNonterminals grammar,
        rules           = Rules $ findRules grammar,
        startNonterm    = getStartTerminal $ head grammar
    }
