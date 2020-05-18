# Грамматика

Любое правило в грамматике состоит из левой части (в которой обязательно находится нетерминал) и правой части (терминалы/нетерминалы). 

Правила разделяются переносом строки, а вот в самом правиле разделение происходит при помощи пробелов.

``` Haskell
Terminal = [a-z]+
Nonterminal = [A-Z]+

Grammar = Grammar Rule
        | Rule

Rule = LeftPart RightPart

LeftPart = Nonterminal 

RightPart = Nonterminal RightPart 
          | Terminal RightPart
          | Nonterminal
          | Terminal
```