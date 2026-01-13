module Part1.Tasks where

taylor x term n acc eps =
  let next = (-term) * (x ^ 2) / ((n+1) * (n+2)) in
  if abs term <= eps then acc
  else taylor x next (n+2) (acc+term) eps

normalize x =
  let pi2 = 2 * pi in
  let x' = x - pi2 * (fromIntegral . floor $ (x / pi2)) in
  if x' > pi then x' - pi2 else x'

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = let nx = normalize x in taylor nx nx 1 0 1e-6

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = taylor (normalize x) 1 0 0 1e-6

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD lhs rhs =
  if rhs == 0 then abs lhs
  else myGCD rhs (lhs `mod` rhs)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year =
  let
    february
      | year `mod` 400 == 0 = 29
      | year `mod` 100 == 0 = 28
      | year `mod` 4 == 0   = 29
      | otherwise           = 28
    dayOk
      | elem month [1, 3, 5, 7, 8, 10, 12] = day <= 31
      | elem month [4, 6, 9, 11]           = day <= 30
      | month == 2                         = day <= february
      | otherwise                          = False
  in (year >= 0) && (day >= 1) && dayOk

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a 0 = 1
myPow a b =
  if even b
    then myPow (a ^ 2) (b `div` 2)
    else a * myPow a (b - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = noDivisors n 2
  where
    noDivisors n d
      | (d ^ 2) > n      = True
      | (n `mod` d) == 0 = False
      | otherwise        = noDivisors n (d + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  let f (a,b) (c,d) = a*d - b*c
  in abs (sum $ zipWith f points $ tail $ cycle points) / 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
  let
    maxFirst3 a b c =
      if a > b then
        if a > c then (a, b, c) else (c, b, a)
      else
        if b > c then (b, a, c) else (c, b, a)
    (hypot, b', c') = maxFirst3 a b c in
  if hypot > b' + c' then -1
  else
    let hypotSq = hypot ^ 2
        bcSq = (b' ^ 2) + (c' ^ 2) in
    if hypotSq == bcSq then 2
    else if hypotSq < bcSq then 1
    else 0
