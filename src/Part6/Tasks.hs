{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    mmul :: mx -> mx -> mx

    mat :: mx -> Int -> Int -> Int
    mminor :: mx -> Int -> Int -> mx

    msize :: mx -> (Int, Int)

    mE :: Int -> mx
    mZero :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  mmul = (*)

  mat n _ _ = n
  mminor n _ _ = n

  msize _ = (1, 1)

  mE _  = 1
  mZero _ _ = 0

instance Matrix [[Int]] where
    mmul l r =
        let (n, _) = msize l in
        let (_, m) = msize r in
        let row m i = m !! i in
        let col m i = fmap (!! i) m in
        let smul l r = sum $ fmap (\(le, re) -> le * re) $ zip l r in
        fmap (\i ->
            fmap (\j ->
                smul (row l i) (col r j)
                ) [0 .. m - 1]
            ) [0 .. n - 1]

    mat l i j = l !! i !! j
    mminor l i j =
        let (n, m) = msize l in
        let rs = fmap snd $ Prelude.filter (\(ix, _) -> ix /= i) $ zip [0 .. n - 1] l in
        fmap (\r -> fmap snd $ Prelude.filter (\(ix, _) -> ix /= j) $ zip [0 .. m - 1] r) rs

    msize [] = (0, 0)
    msize l = (length l, length $ l !! 0)

    mE n =
        let _1at i = fmap (\ix -> if i == ix then 1 else 0) [0 .. n - 1] in
        fmap _1at [0 .. n - 1]

    mZero n m = replicate n $ replicate m 0

get k m = case Data.Map.lookup k m of
        Just n -> n
        Nothing -> 0

instance Matrix (SparseMatrix Int) where
    mE n = SparseMatrix n n $ Prelude.foldl (\m i -> insert (i, i) 1 m) Data.Map.empty [0 .. n - 1]

    mmul (SparseMatrix lw lh lmap) (SparseMatrix rw _ rmap) =
        let pairs = concat $ fmap (\i -> fmap (\j -> (i, j)) [0 .. rw - 1]) [0 .. lh - 1] in
        let m' = Prelude.foldl (\m (i, j) ->
                    let row = fmap (\ix -> get (i, ix) lmap) [0 .. lw - 1] in
                    let col = fmap (\ix -> get (ix, j) rmap) [0 .. lw - 1] in
                    let e = sum $ fmap (\(x, y) -> x * y) $ zip row col in
                    if e == 0 then m
                    else insert (i, j) e m
                )
                Data.Map.empty pairs in
        SparseMatrix rw lh m'
    mat (SparseMatrix _ _ m) i j = get (i, j) m

    mminor (SparseMatrix w h m) i j =
        let wr = Prelude.foldl (\m ix -> delete (ix, j) m) m [0 .. h - 1] in
        let m' = Prelude.foldl (\m ix -> delete (i, ix) m) wr [0 .. w - 1] in
        let shift k cut = if k > cut then k - 1 else k in
        SparseMatrix (w - 1) (h - 1) $ Data.Map.mapKeys (\(ri, ci) -> (shift ri i, shift ci j)) m'

    msize (SparseMatrix m n _) = (n, m)
    mZero n m = SparseMatrix m n Data.Map.empty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = mE

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero m n = mZero n m

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = mmul

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m =
    let (n, _) = msize m in
    if n == 1 then mat m 0 0
    else
        let sign j = if j `mod` 2 == 0 then 1 else -1 in
        let s j = (sign j) * (mat m 0 j) * (determinant $ mminor m 0 j) in
        sum $ fmap s [0 .. n - 1]
