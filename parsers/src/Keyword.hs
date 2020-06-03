module Keyword where

import Combinators (Parser, symbol, elem')
import Control.Applicative
import Control.Monad
import Data.Bool
import qualified Data.Map.Strict as Map


-- Структура данных БОР и его методы
data Trie = Trie Bool (Map.Map Char Trie)
    deriving (Show)

isTerminal :: Trie -> Bool
isTerminal (Trie terminal children) = terminal

add :: Trie -> String -> Trie
add (Trie terminal children) []         = Trie True children
add (Trie terminal children) (sym:syms) = Trie terminal (Map.alter (Just . helper) sym children)
    where
        helper (Just children)  = add children syms
        helper Nothing          = add (Trie False Map.empty) syms

build :: [String] -> Trie
build = foldr (flip add) (Trie False Map.empty)

find :: Trie -> Char -> Maybe Trie
find (Trie terminal children) symbol = Map.lookup symbol children
-- ^^^ структура БОР


-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
keyword :: [String] -> Parser String String String
keyword ks = reverse <$> helper (build ks) []
    where
        eof = do
            result <- many elem'
            if null result 
            then pure () 
            else fail "Error"
        out x = void (symbol ' ') <|> void (symbol '\n') <|> eof >> return x
        helper trie word = do
            sym <- elem'
            case find trie sym of
                Just trie -> bool (helper trie (sym:word)) (helper trie (sym:word) <|> out (sym:word)) (isTerminal trie)
                Nothing   -> fail "Error"
