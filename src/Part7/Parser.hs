module Part7.Parser where

import Control.Applicative (Alternative (..))
import Util (notImplementedYet)

{-
Парсер представляет из себя функцию из входной строки в один из двух вариантов
1. Остаток строки после разбора и сам результат разбора
2. Строковую ошибку
-}
newtype Parser a = Parser {runParser :: String -> Either String (String, a)}

instance Functor Parser where
  fmap f p = Parser $ \s -> case runParser p s of
    Left e -> Left e
    Right (s, v) -> Right (s, f v)

instance Applicative Parser where
  pure v = Parser $ \s -> Right (s, v)

  (<*>) f p = Parser $ \s -> case runParser f s of
    Left e -> Left e
    Right (s, f) -> case runParser p s of
      Left e -> Left e
      Right (s, v) -> Right (s, f v)

instance Alternative Parser where
  (<|>) l r = Parser $ \s -> case runParser l s of
    Left _ -> runParser r s
    Right v -> Right v

  empty = Parser $ \s -> Left "empty parser"

instance Monad Parser where
  (>>=) p f = Parser $ \s -> case runParser p s of
    Left e -> Left e
    Right (s, v) ->
      let (Parser p) = f v
       in p s

instance MonadFail Parser where
  fail e = Parser $ \s -> Left e

unexpectedEndFail = Left "unexpected end of string"

stringIsNotEmptyFail = Left "string is not empty"

expectedSybolCFail c = Left $ '[' : c : ']' : " symbol expected here"

expectCharByPredicateFail = Left "expected char that satisfies predicate"

canNotParsePatternFail pattern = Left $ "can not parse: " ++ pattern

-- | Проверяет что входная строка закончилась, иначе - ошибка
eofP :: Parser ()
eofP = Parser $ \s -> case s of
  [] -> Right ([], ())
  _ -> stringIsNotEmptyFail

-- | Пытается прочитать заданный символ, если не получилось - ошибка
charP :: Char -> Parser ()
charP c = Parser $ \s -> case s of
  [] -> unexpectedEndFail
  hd : tl | hd == c -> Right (tl, ())
  _ -> expectedSybolCFail c

-- | Пытается прочитать любой символ, если не получилось - ошибка
anyCharP :: Parser Char
anyCharP = Parser $ \s -> case s of
  [] -> unexpectedEndFail
  hd : tl -> Right (tl, hd)

-- | Пытается прочитать заданную строку, если не получилось - ошибка
stringP :: String -> Parser ()
stringP pattern =
  let cut l r = case (l, r) of
        ([], l) -> Right (l, ())
        (_, []) -> unexpectedEndFail
        (lhd : ltl, rhd : rtl) | lhd == rhd -> cut ltl rtl
        _ -> canNotParsePatternFail pattern
   in Parser $ \s -> cut pattern s

-- | Парсит 0 и больше раз `p` разделенный `sepP`.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sepP = sepBy1 p sepP <|> return []

-- | Парсит 1 и больше раз `p` разделенный `sepP`.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sepP = Parser $ \s -> case runParser p s of
  Left e -> Left e
  Right (s, v) ->
    let helper (vs, s) = case runParser (sepP *> p) s of
          Left _ -> (vs, s)
          Right (s, v) -> helper (v : vs, s)
     in let (vs, tl) = helper ([v], s)
         in Right (tl, reverse vs)

-- | Парсит p, если не получилось возвращает дефолтное значение
optional :: a -> Parser a -> Parser a
optional def p = p <|> return def

parseAlts :: [Parser a] -> Parser a
parseAlts ps = foldl (<|>) empty ps

manyP :: Parser a -> Parser [a]
manyP p = sepBy p $ return []

manyP1 :: Parser a -> Parser [a]
manyP1 p = sepBy1 p $ return []

groupP :: [String] -> Parser ()
groupP kws = parseAlts $ map stringP kws

charPP :: (Char -> Bool) -> Parser Char
charPP f = Parser $ \s -> case s of
  [] -> unexpectedEndFail
  hd : tl | f hd -> Right (tl, hd)
  _ -> expectCharByPredicateFail

(|>) v f = f v

infixl 2 |>
