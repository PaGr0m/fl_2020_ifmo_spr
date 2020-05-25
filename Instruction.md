## Запуск alex и happy
**alex Lexer.x && happy Parser.y && stack ghci Parser.hs**

### Parse error:
- parse (Lexer.alexScanTokens "S S S")
- parse (Lexer.alexScanTokens "S #")
- parse (Lexer.alexScanTokens "S #; S S s")

### Parse
- parse (Lexer.alexScanTokens "S #;")
- parse (Lexer.alexScanTokens "S #; S S s;")

---

## Запуск через ghci
1) ghci Converter.hs 
2) grammarConverter $ Parser.parse (Lexer.alexScanTokens "S #; S S s; NONTERMINAL terminal;")

### Parse error
- grammarConverter $ Parser.parse (Lexer.alexScanTokens "S #; S S s; NONTERMINAL terminal")