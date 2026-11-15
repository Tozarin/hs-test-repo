{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist [] = REmpty
listToRlist rl =
    let helper acc [] = acc
        helper acc (hd : tl) = helper (acc :< hd) tl in
    helper REmpty rl

fold f acc REmpty = acc
fold f acc (tl :< hd) = fold f (f acc hd) tl

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ rl = showString $ show rl
    show rl =
        let helper REmpty = ""
            helper (REmpty :< hd) = show hd
            helper (tl :< hd) = helper tl ++ "," ++ show hd in
        "[" ++ helper rl ++ "]"

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty r = False
    (==) l REmpty = False
    (==) (ltl :< lhd) (rtl :< rhd) = if lhd /= rhd then False else lhd == rhd

    (/=) l r = not $ (l == r)

instance Semigroup (ReverseList a) where
    (<>) l REmpty = l
    (<>) l r =
        let helper REmpty = REmpty
            helper (REmpty :< hd) = l :< hd
            helper (tl :< hd) = helper tl :< hd
        in
        helper r

instance Monoid (ReverseList a) where
  mempty = REmpty
  mappend = (<>)
  mconcat = foldl (<>) REmpty

instance Functor ReverseList where
  fmap f REmpty = REmpty
  fmap f (tl :< hd) = fmap f tl :< f hd

instance Applicative ReverseList where
  pure e = REmpty :< e
  (<*>) fs es =
    let l = fmap (\f -> fmap f es) fs in
    fold (\l r -> r <> l) REmpty l

instance Monad ReverseList where
  (>>=) REmpty _ = REmpty
  (>>=) rl f =
    let l = fmap f rl in
    fold (\l r -> r <> l) REmpty l

  return = pure
