module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (hd : tl) = myFoldl f (f acc hd) tl

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc l =
    let helper (hd : []) = f hd acc
        helper (hd : tl) = f hd $ helper tl in
    helper l

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\e tl -> f e : tl) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f l = myConcat $ myMap f l

glue :: [a] -> [a] -> [a]
glue l r = foldr (:) r l

myConcat :: [[a]] -> [a]
myConcat = foldl glue []

myReverse :: [a] -> [a]
myReverse l = foldl (\acc e -> e : acc) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\e tl -> if p e then e : tl else tl) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p l = (myFilter p l, myFilter (not . p) l)
