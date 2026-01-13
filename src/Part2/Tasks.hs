module Part2.Tasks where

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar name new old@(Variable name') = if name == name' then new else old
replaceVar name new (BinaryTerm op a b) = BinaryTerm op (replaceVar name new a) (replaceVar name new b)
replaceVar _ _ old = old

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate t@(IntConstant _) = t
evaluate t@(Variable _)    = t
evaluate (BinaryTerm op a b) =
  evalBinary (evaluate a) (evaluate b)
  where
    evalBinary (IntConstant a) (IntConstant b) =
      IntConstant (evalConstant op a b)
    evalBinary a b = BinaryTerm op a b
    evalConstant op a b = case op of
      Plus -> a + b
      Minus -> a - b
      Times -> a * b