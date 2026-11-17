module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    IntConstant _ -> expression
    Variable name | name == varName -> replacement
    Variable _ -> expression
    BinaryTerm op lhv rhv -> BinaryTerm op (replace lhv) (replace rhv)
    where
        replace = replaceVar varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate term =
    case term of
    BinaryTerm op lhv rhv ->
        let lhv' = evaluate lhv in
        let rhv' = evaluate rhv in
        let binop :: BinaryOp -> Int -> Int -> Int
            binop op = case op of
                Plus -> (+)
                Minus -> (-)
                Times -> (*)
        in
        case (lhv', rhv') of
        (IntConstant l, IntConstant r) -> IntConstant $ binop op l r
        otherwise -> BinaryTerm op lhv' rhv'
    otherwise -> term
