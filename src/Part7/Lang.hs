module Part7.Lang where

import Control.Applicative (Alternative (..))
import Part7.Parser

data Identifyier = Id {id :: String} deriving (Show, Eq)

data Lang = Programm {instructions :: [LangInstruction]} deriving (Show, Eq)

data LangInstruction
  = Binding {bname :: Identifyier, term :: LTerm}
  | Expr {term :: LTerm}
  deriving (Show, Eq)

data LTerm
  = Variable {vname :: Identifyier}
  | Application {f :: LTerm, value :: LTerm}
  | Abstraction {arg :: Identifyier, body :: LTerm}
  deriving (Show, Eq)

parse :: String -> Either String Lang
parse s = case runParser programmP s of
  Left e -> Left e
  Right (_, p) -> Right p

whitespaces = [" ", "\t"]

endOfLine = "\n"

lambdaKwS = ["fun", "lambda"]

lambdaSymS = ["\\", "λ", "Λ", "l", "L"]

lambdaS = lambdaKwS ++ lambdaSymS

arrowS = ["->", "=>", "arrow", ".", ":"]

bindingS = ["let"]

eqS = ["=", ":="]

leftParentS = ["("]

rightParentS = [")"]

banned =
  concat
    [ whitespaces,
      [endOfLine],
      lambdaS,
      arrowS,
      bindingS,
      eqS,
      leftParentS,
      rightParentS
    ]

idS = '_' : '\'' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

identifyierFail = fail "fail parse identifyier" :: Parser a

expectedBindingKwFail = fail "expected binding keyword" :: Parser a

expectedLambdaKwFail = fail "expected lambda keyword" :: Parser a

ws = groupP whitespaces |> manyP

endOfLineP = stringP endOfLine

skipWs p = ws *> p <* ws

lineP p = skipWs p <* (endOfLineP <|> eofP)

emptyLinesP = manyP (ws *> endOfLineP)

keywordP kws err =
  skipWs wordP >>= \w ->
    if elem w kws
      then return ()
      else err

wordP = charPP (\c -> elem c idS) |> manyP1

programmP =
  Programm <$> (emptyLinesP *> (lineP instrP <* emptyLinesP |> manyP1)) <* eofP

instrP = bindingP <|> exprP

bindindWordP = keywordP bindingS expectedBindingKwFail

bindingEqP = groupP eqS |> skipWs

identifyierP =
  skipWs wordP >>= \w ->
    if elem w banned
      then identifyierFail
      else return $ Id w

bindingP =
  bindindWordP *> identifyierP >>= \id ->
    bindingEqP *> termP |> (<$>) (Binding id)

exprP = Expr <$> termP

varP = Variable <$> identifyierP

lambdaP = keywordP lambdaKwS expectedLambdaKwFail <|> groupP lambdaSymS |> skipWs

arrowP = groupP arrowS |> skipWs

absP =
  lambdaP *> identifyierP >>= \id ->
    arrowP *> termP |> (<$>) (Abstraction id)

leftParentP = groupP leftParentS |> skipWs

rightParentP = groupP rightParentS |> skipWs

parentsP p = leftParentP *> p <* rightParentP

atomP = varP <|> absP <|> parentsP termP

termP = foldl1 Application <$> manyP1 atomP
