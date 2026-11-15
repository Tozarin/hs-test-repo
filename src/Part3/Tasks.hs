module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n ..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = iterate f x

updateAt :: (Int -> Int) -> Int -> [Int] -> [Int]
updateAt f i ns =
    let helper n [] = []
        helper n (hd : tl) =
            let hd' = if n == i then f hd else hd in
            hd' : helper (n + 1) tl
    in helper 0 ns

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq ns =
    let nums :: [Int] -> Int -> [Int]
        nums acc n = if n < 10 then updateAt (+ 1) n acc
        else nums (updateAt (+ 1) (n `mod` 10) acc) (n `div` 10)
    in
    let stat = foldl nums (replicate 10 0) ns in

    let (_, mi, _) = foldl (\(i, mi, mc) c -> if c > mc then (i + 1, i, c) else (i + 1, mi, mc)) (0, 0, -1) stat in
    mi

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\seen e -> if elem e seen then seen else e : seen) []

appendIn :: (Eq k) => k -> a -> [(k, [a])] -> [(k, [a])]
appendIn key e [] = [(key, [e])]
appendIn key e ((k, es) : tl) =
    if key == k then (k, e : es) : tl
    else (k, es) : appendIn key e tl

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f = foldl (\gs v -> appendIn (f v) v gs) []
