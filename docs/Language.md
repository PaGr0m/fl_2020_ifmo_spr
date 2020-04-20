# Language

```
PROGRAM ::= VARIABLE_DECLARATION COMPUTATION
VARIABLE_DECLARATION ::= "Var" VARIABLE_LIST ';'
VARIABLE_LIST ::= IDENT | VARIABLE_LIST

COMPUTATION ::= ASSIGNMENT_LIST 
              | BRANCHING 
              | CYCLE
              | ASSIGNMENT_LIST COMPUTATION
              | BRANCHING COMPUTATION
              | CYCLE COMPUTATION

BRANCHING ::= IF 
            | IF ELSE 
            | IF ELSE_IF ELSE
            | IF BRANCHING
            | IF ELSE BRANCHING
            | IF ELSE_IF ELSE BRANCHING

IF ::= "if" '(' EXPRESSION ')' '{' COMPUTATION '}' 
ELSE_IF ::= "else if" '(' EXPRESSION ')' '{' COMPUTATION '}'
ELSE ::= "else" '{' COMPUTATION '}'

CYCLE_KEYWORD = "while" | "for"
CYCLE ::= CYCLE_KEYWORD '(' EXPRESSION ')' '{' COMPUTATION '}' 
        | CYCLE_KEYWORD '(' EXPRESSION ')' '{' COMPUTATION '}' CYCLE

ASSIGNMENT_LIST ::= ASSIGNMENT | ASSIGNMENT ASSIGNMENT_LIST
ASSIGNMENT ::= IDENT '=' EXPRESSION ';'
EXPRESSION ::= UNARY_OP SUBEXPRESSION | SUBEXPRESSION
SUBEXPRESSION ::= '(' EXPRESSION ')' | OPERAND | SUBEXPRESSION BINARY_OP SUBEXPRESSION
OPERAND ::= IDENT | CONSTANT

BINARY_OP ::= "+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||"
UNARY_OP ::= '-'

IDENT ::= LETTER IDENT | LETTER
CONSTANT ::= DIGIT CONSTANT | DIGIT
LETTER ::= [a-zA-Z]
DIGIT ::= [0-9]
```

#### PROGRAM 
Программа состоит из двух блоков :
* Блока объявления переменных 
* Вычислений

Блок объявлений начинается с ключевого слова "Var" и далее записывается список переменных. Список переменных - это просто иденты.

Под вычислением может быть одно из следующих:
 * Список присваиваний
 * Ветвление
 * Цикл

Под списком присваиваний подразумеваются арифметические операции, результат которых присваивается иденту.

Ветвления: блок кода, который выполняется, если EXPRESSION является не 0.

Цикл: блок кода, который выполняется пока верен EXPRESSION.

Идент может состоять только из букв, а число только из цифр.

Пробелы могут быть от 1 и более, но только между лексемами.

#### Function
Функция начинается с ключевого слова `fun` после идут аргументы функции в круглых скобках, и далее тело в фигурных.

#### Program
Программа состоит из функций, где обязательно должна быть `main`.
