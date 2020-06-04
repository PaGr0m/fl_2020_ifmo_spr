{-# LANGUAGE LambdaCase #-}

module Combinators where

import AST                 (AST (..), Operator (..))
import Control.Applicative (Alternative (..))
import Text.Printf         (printf)


data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result}

instance (Monoid error, Read error) => Functor (Parser error input) where
  -- В случае успешного разбора модифицирует результат при помощи функции f
  -- fmap :: (a -> b) -> Parser e i a -> Parser e i b
  fmap f p = Parser $ \input ->
        case runParser p input of
          Success input' result -> Success input' (f result)
          Failure err   -> Failure err

instance (Monoid error, Read error) => Applicative (Parser error input) where
  pure = return
  p <*> q = Parser $ \input ->
    case runParser p input of
      Success input' f ->
        case runParser q input' of
          Success input'' a -> Success input'' (f a)
          Failure err -> Failure err
      Failure err -> Failure err

instance (Monoid error, Read error) => Monad (Parser error input) where
  -- Всегда завершается успехом, вход не читает, возвращает данное значение
  return res = Parser $ \input -> Success input res

  -- Последовательное применение парсеров:
  -- если первый парсер успешно принимает префикс строки, второй запускается на суффиксе.
  -- Второй парсер использует результат первого.
  p >>= x = Parser $ \input ->
    case runParser p input of
      Success input' y -> runParser (x y) input'
      Failure e -> Failure e

instance (Monoid error, Read error) => Alternative (Parser error input) where
  empty = Parser (\_ -> Failure mempty)

  -- Альтернатива: в случае неудачи разбора первым парсером, парсит вторым
  p <|> x = Parser $ \input ->
    case runParser p input of 
      Failure _ -> runParser x input
      result    -> result

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: (Monoid e, Read e) => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> many' (sep *> elem)

sepBy1' :: (Monoid e, Read e) => Parser e i t1 -> Parser e i t -> Parser e i (t, [(t1, t)])
sepBy1' sep elem = do
  value <- elem
  values <- many' (flip fmap elem . (,) =<< sep) <|> return []
  return (value, values)

sepBy1'' :: (Monoid e, Read e) => Parser e i t1 -> Parser e i t -> Parser e i (t, [(t1, t)])
sepBy1'' sep elem = do
    values <- many (flip fmap sep . (,) =<< elem) <|> return []
    value <- elem
    return (value, fmap (uncurry $ flip (,)) values)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \case
    (x : xs) | p x -> Success xs x
    input -> Failure "Predicate failed"

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (== c)

symbols :: String -> Parser String String String
symbols = foldr (\ x -> (<*>) ((:) <$> satisfy (== x))) (return [])

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: (Monoid e, Read e) =>  Parser e i a -> Parser e i [a]
many' p = some' p <|> return []

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: (Monoid e, Read e) => Parser e i a -> Parser e i [a]
some' p = do 
  res <- p
  (res:) <$> (some' p <|> return [])

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e
