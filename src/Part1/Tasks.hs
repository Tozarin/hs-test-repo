module Part1.Tasks where

import Util(notImplementedYet)
import GHC.Base (VecElem(Int16ElemRep))

fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n =
    let helper 0 p = p
        helper 1 p = p
        helper n p = helper (n - 1) (n * p)
    in helper n 1

tay :: (Integer -> Integer) -> Integer -> Double -> Double
tay shift n x =
    let helper :: Integer -> Double -> Double
        helper (-1) s = s
        helper n s =
            let c = x ** fromIntegral (shift $ 2 * n) in
            let z = fac (shift $ 2 * n) in
            let sign = if n `mod` 2 == 0 then 1 else -1 in
            helper (n - 1) (s + sign * c / fromInteger z)
    in helper n 0

taySin :: Integer -> Double -> Double
taySin = tay $ (+) 1

toPiPi :: Double -> Double
toPiPi x =
    let piPiCount = fromIntegral $ round (x / (2 * pi)) in
    x - 2 * pi * piPiCount

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = taySin 7 $ toPiPi x

tayCos :: Integer -> Double -> Double
tayCos = tay id

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = tayCos 7 $ toPiPi x

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD 0 y = abs y
myGCD x 0 = abs x
myGCD x y =
    let (xm, ym) = (abs x, abs y) in
    if xm > ym then myGCD ym (xm `mod` ym) else myGCD xm (ym `mod` xm)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y =
    let is31 = elem m [1, 3, 5, 7, 8, 10, 12] in
    let is30 = elem m [4, 6, 9, 11] in
    let is31Good = is31 && elem d [1 .. 31] in
    let is30Good = is30 && elem d [1 .. 30] in

    let isFeb = m == 2 in
    let isV | y `mod` 400 == 0 = True
            | y `mod` 100 == 0 = False
            | y `mod` 4 == 0 = True
            | otherwise = False
    in
    let febDaysC = if isV then 29 else 28 in
    let isFebGood = isFeb && elem d [1 .. febDaysC] in

    is31Good || is30Good || isFebGood

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n 0 = 1
myPow n 1 = n
myPow n p =
    let pow = myPow n (p `div` 2) in
    pow * pow * if p `mod` 2 == 0 then 1 else n

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n =
    let sn = (+) 1 $ ceiling $ sqrt $ fromIntegral n in
    let helper ni | ni > sn = True
                  | n `mod` ni == 0 = False
                  | otherwise = helper $ ni + 1
    in
    helper 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea ps =
    let foldL ((xi', yi'), s) (xi, yi) = ((xi, yi), s + xi' * yi - xi * yi') in
    let (x1, y1) : tl = ps in
    let ((xn, yn), s) = foldl foldL ((x1, y1), 0) tl in
    let sum = s + xn * y1 - x1 * yn in
    (abs sum) / 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | any (\(a, b, c) -> a + b <= c) allT = -1
                   | any (\x -> -gate < x && x < gate) allC = 2
                   | all (> gate) allC = 1
                   | any (< -gate) allC = 0
                   | otherwise = -1
    where
        gate = 0.001
        cs (a, b, c) = (b * b + c * c - a * a) / (2 * b * c)
        allT = [(a, b, c), (b, a, c), (c, a, b)]
        allC = map cs allT
