module Part7.Tests where

import Control.Applicative (Alternative (..))
import Control.Monad (Monad (..))
import Part7.Parser
import Test.Tasty.HUnit

(|>) v f = f v

infixl 2 |>

run :: String -> Parser a -> Either String (String, a)
run s p = runParser p s

applyE :: (t -> b) -> Either String (String, t) -> Either String (String, b)
applyE _ (Left e) = Left e
applyE f (Right (s, v)) = Right (s, f v)

unit_parser_functor_id = do
  compare $ return "x"
  compare $ charP 'Y'
  compare $ stringP s
  where
    s = "some string"
    compare p =
      run s (fmap id p) @?= run s p |> applyE id

unit_parser_functor_compose = do
  fmap (f . g) p |> run s @?= fmap g p |> fmap f |> run s
  where
    s = "123"
    p = anyCharP
    f = (+) 1
    g = fromEnum

unit_parser_applicative_id = do
  compare $ return "x"
  compare $ charP 'Y'
  compare $ stringP s
  where
    s = "some string"
    compare p = pure id <*> p |> run s @?= run s p

unit_parser_applicative_homo = do
  pure f <*> pure x |> run s @?= pure (f x) |> run s
  where
    s = "123"
    x = 'a'
    f = fromEnum

unit_parser_alternative_idem = do
  empty <|> p |> run s @?= run s p
  p <|> empty |> run s @?= run s p
  where
    s = "some string"
    p = anyCharP

unit_parser_alternative_dist = do
  compare (pure f <*> (x <|> y)) ((pure f <*> x) <|> (pure f <*> y))
  where
    s = "some string"
    x = pure 'a'
    y = pure 'y'
    f = fromEnum
    compare l r = run s l @?= run s r

unit_parser_monad_left_id = do
  (return x >>= f) |> run s @?= f x |> run s
  where
    s = "some string"
    f = \c -> return $ fromEnum c
    x = 'a'

unit_parser_monad_right_id = do
  (p >>= return) |> run s @?= run s p
  where
    s = "some string"
    p = anyCharP

unit_parser_monad_ass = do
  ((p >>= f) >>= g) |> run s @?= (p >>= (\x -> f x >>= g)) |> run s
  where
    s = "some string"
    p = anyCharP
    f = \s -> return $ fromEnum s
    g = \c -> return $ c + 1
