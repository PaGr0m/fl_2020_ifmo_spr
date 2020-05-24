# Грамматика

Любое правило в грамматике состоит из левой части (в которой обязательно находится нетерминал) и правой части (терминалы/нетерминалы). 

Правила разделяются специальным символом `;`. А вот в самом правиле разделение происходит при помощи пробелов. 

В данной грамматике символ `#` выступает в качестве эпсилон.

``` Haskell
Terminal = [a-z]+
Nonterminal = [A-Z]+

Grammar = Rules

Rules = Rule Rules
      | Rule 

Rule = LeftPart RightPart;

LeftPart = Nonterminal 

RightPart = #
          | ActualRightPart

ActualRightPart = Nonterminal ActualRightPart 
                | Terminal ActualRightPart
                | Nonterminal
                | Terminal
```