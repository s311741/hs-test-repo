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
  rows :: mx -> Int
  cols :: mx -> Int
  zeroM :: Int -> Int -> mx
  unitM :: Int -> mx
  ofM :: Int -> Int -> (Int -> Int -> Int) -> mx
  at :: Int -> Int -> mx -> Int
  subM :: Int -> Int -> mx -> mx

zrange n = [0..n-1]

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  rows _ = 1
  cols _ = 1
  zeroM 1 1 = 0
  unitM 1 = 1
  ofM 1 1 f = f 0 0
  at 0 0 = id
  subM 0 0 = id

instance Matrix [[Int]] where
  rows = length
  cols = length . head
  zeroM w h = replicate h $ replicate w 0
  unitM w = [[if i == j then 1 else 0 | j <- zrange w] | i <- zrange w]
  at row col m = m !! row !! col
  subM row col m =
    [ [m !! r !! c | c <- zrange (cols m), c /= col]
      | r <- zrange (rows m), r /= row ]

instance Matrix (SparseMatrix Int) where
  rows = sparseMatrixHeight
  cols = sparseMatrixWidth
  zeroM w h = SparseMatrix w h mempty
  unitM w = SparseMatrix w w $ fromList [((i, i), 1) | i <- zrange w]
  at row col m = findWithDefault 0 (row, col) (sparseMatrixElements m)
  subM row col m =
    SparseMatrix
      (cols m - 1)
      (rows m - 1)
      (fromList
        [((r', c'), v) |
          ((r, c), v) <- toList (sparseMatrixElements m), r /= row, c /= col,
          let r' = if r > row then r-1 else r, let c' = if c > col then c-1 else c])


-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = unitM w

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = zeroM w h

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b =
  let f x y = sum [(at y k a) * (at k x b) | k <- zrange (cols a)]
  in ofM (cols b) (rows a) f

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m =
  let sign n = if even n then 1 else -1 in
  if cols m == 1 then at 0 0 m
  else sum [(sign k) * (at 0 k m) * determinant (subM 0 k m) | k <- zrange (cols m)]
