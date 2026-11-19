module Part7.Tests where

import Control.Applicative (Alternative (..))
import Control.Monad (Monad (..))
import Part7.Lang
import Part7.Parser
import Test.Tasty.HUnit

run :: String -> Parser a -> Either String (String, a)
run s p = runParser p s

applyE :: (t -> b) -> Either String (String, t) -> Either String (String, b)
applyE _ (Left e) = Left e
applyE f (Right (s, v)) = Right (s, f v)

-- fmap id == id
unit_parser_functor_id = do
  compare $ return "x"
  compare $ charP 'Y'
  compare $ stringP s
  where
    s = "some string"
    compare p =
      run s (fmap Prelude.id p) @?= run s p |> applyE Prelude.id

-- fmap (f . g) == fmap g . fmap f
unit_parser_functor_compose = do
  fmap (f . g) p |> run s @?= fmap g p |> fmap f |> run s
  where
    s = "123"
    p = anyCharP
    f = (+) 1
    g = fromEnum

-- pure id <*> x == x
unit_parser_applicative_id = do
  compare $ return "x"
  compare $ charP 'Y'
  compare $ stringP s
  where
    s = "some string"
    compare p = pure Prelude.id <*> p |> run s @?= run s p

-- pure f <*> pure x == pure (f x)
unit_parser_applicative_homo = do
  pure f <*> pure x |> run s @?= pure (f x) |> run s
  where
    s = "123"
    x = 'a'
    f = fromEnum

-- empty <|> x == x
-- x <|> empty == x
unit_parser_alternative_idem = do
  empty <|> p |> run s @?= run s p
  p <|> empty |> run s @?= run s p
  where
    s = "some string"
    p = anyCharP

-- pure f <*> (x <|> y) == (pure f <*> x) <|> (pure f <*> y)
unit_parser_alternative_dist = do
  compare (pure f <*> (x <|> y)) ((pure f <*> x) <|> (pure f <*> y))
  where
    s = "some string"
    x = pure 'a'
    y = pure 'y'
    f = fromEnum
    compare l r = run s l @?= run s r

-- return x >>= f == f x
unit_parser_monad_left_id = do
  (return x >>= f) |> run s @?= f x |> run s
  where
    s = "some string"
    f = \c -> return $ fromEnum c
    x = 'a'

-- x >>= return == x
unit_parser_monad_right_id = do
  (p >>= return) |> run s @?= run s p
  where
    s = "some string"
    p = anyCharP

-- (x >>= f) >>= g == x >>= \x -> f x >>= g
unit_parser_monad_ass = do
  ((p >>= f) >>= g) |> run s @?= (p >>= (\x -> f x >>= g)) |> run s
  where
    s = "some string"
    p = anyCharP
    f = \s -> return $ fromEnum s
    g = \c -> return $ c + 1

unit_parser_eof = do
  run "" eofP @?= Right ([], ())
  run "empty" eofP @?= stringIsNotEmptyFail

unit_parser_charp = do
  charP c |> run "" @?= unexpectedEndFail
  charP c |> run (c : tl) @?= Right (tl, ())
  charP c |> run "cba" @?= expectedSybolCFail c
  where
    c = 'a'
    tl = "bc"

unit_parser_anycharp = do
  run "" anyCharP @?= unexpectedEndFail
  run (hd : tl) anyCharP @?= Right (tl, hd)
  where
    hd = 'a'
    tl = "bc"

unit_parser_stringp = do
  stringP pattern |> run "" @?= unexpectedEndFail
  stringP pattern |> run (pattern ++ tl) @?= Right (tl, ())
  stringP pattern |> run tl @?= canNotParsePatternFail pattern
  where
    pattern = "abc"
    tl = "def"

unit_parser_sepby = do
  sepBy p sepP |> run "" @?= Right ("", [])
  sepBy p sepP |> run bs @?= Right (bs, [])
  sepBy p sepP |> run "a" @?= Right ("", [()])
  sepBy p sepP |> run "a,a,a,a" @?= Right ("", replicate 4 ())
  sepBy p sepP |> run "a,a,a," @?= Right (",", replicate 3 ())
  where
    bs = "b,b,b,b"
    p = charP 'a'
    sepP = charP ','

unit_parser_sepby1 = do
  sepBy1 p sepP |> run "" @?= unexpectedEndFail
  sepBy1 p sepP |> run bs @?= expectedSybolCFail 'a'
  sepBy1 p sepP |> run "a" @?= Right ("", [()])
  sepBy1 p sepP |> run "a,a,a,a" @?= Right ("", replicate 4 ())
  sepBy1 p sepP |> run "a,a,a," @?= Right (",", replicate 3 ())
  where
    bs = "b,b,b,b"
    p = charP 'a'
    sepP = charP ','

unit_parser_optional = do
  optional n p |> run "" @?= Right ("", n)
  optional n p |> run [c] @?= Right ("", c)
  where
    c = 'b'
    n = 'a'
    p = anyCharP

v n = n |> Id |> Variable

unit_parser_lambda_keywords = do
  parse "\\x -> x" @?= Right p
  parse "fun x => x" @?= Right p
  parse "lambda x . x" @?= Right p
  parse "Λ x arrow x" @?= Right p
  where
    p = Programm [Expr $ Abstraction (Id "x") (v "x")]

unit_parser_application = do
  parse "f x" @?= Right (p $ Application (v "f") (v "x"))
  parse "f x y" @?= Right (p $ Application (Application (v "f") (v "x")) (v "y"))
  parse "f (x y)" @?= Right (p $ Application (v "f") (Application (v "x") (v "y")))
  parse "(\\x -> x) y" @?= Right (p $ Application (Abstraction (Id "x") (v "x")) (v "y"))
  where
    p t = Programm [Expr t]

unit_parser_binding = do
  parse "let id = \\x -> x" @?= Right (p $ Binding (Id "id") (Abstraction (Id "x") (v "x")))
  parse "let k := fun a -> fun b -> a" @?= Right (p $ Binding (Id "k") k)
  where
    p i = Programm [i]
    k = Abstraction (Id "a") (Abstraction (Id "b") (v "a"))

unit_parser_keyword_prefixes = do
  parse "letter x" @?= Right (p $ Application (v "letter") (v "x"))
  parse "function" @?= Right (p $ v "function")
  where
    p t = Programm [Expr t]

unit_parser_multiline = do
  parse "\n let id = \\x -> x   \n\n  \nid y \n" @?= Right p
  where
    p =
      Programm
        [ Binding (Id "id") (Abstraction (Id "x") (v "x")),
          Expr (Application (v "id") (v "y"))
        ]

unit_parser_errors = do
  isLeft (parse "")
  isLeft (parse "let = x")
  isLeft (parse "(x")
  isLeft (parse "\\x -> ")
  isLeft (parse "function -> x")
  where
    isLeft (Left _) = True @?= True
    isLeft _ = False @?= True
