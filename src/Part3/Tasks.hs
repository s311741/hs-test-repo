module Part3.Tasks where

import Data.Char (digitToInt)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [f n] ++ finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = [x] ++ map f (ff f x)

count' _ acc [] = acc
count' x acc (x':xs) = count' x (if x == x' then acc+1 else acc) xs

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq xs =
  let
    digits = map digitToInt $ concat $ map show xs
    update bestDigit bestCount [] = bestDigit
    update bestDigit bestCount (digit:restDigits) =
      let count = count' digit 0 digits in
      if count > bestCount
        then update digit count restDigits
        else update bestDigit bestCount restDigits
  in update 0 0 digits

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) =
  let xs' = uniq xs in
  if elem x xs' then xs' else x:xs'

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]

grokBy f =
  let
    grok' x acc =
      let r = f x
          pred (x', _) = x' == r
      in case break pred acc of
        (_, []) -> (r, [x]) : acc
        (before, (r', xs) : after) -> (r', x:xs):before ++ after
  in foldr grok' []