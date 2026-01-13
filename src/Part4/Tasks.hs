module Part4.Tasks where

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
listToRlist xs = listToRlist' xs REmpty
  where listToRlist' [] acc = acc
        listToRlist' (x:xs) acc = listToRlist' xs (acc:<x)

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
  showsPrec _ xs =
    showChar '[' . (commaSep xs) . showChar ']'
    where
      commaSep REmpty = showString ""
      commaSep (REmpty:<x) = shows x
      commaSep (xs:<x) = commaSep xs . showChar ',' . shows x

instance Eq a => Eq (ReverseList a) where
  REmpty == REmpty = True
  (a:<as) == (b:<bs) = a == b && as == bs
  _ == _ = False

instance Semigroup (ReverseList a) where
  a <> REmpty = a
  a <> (bs:<b) = (a <> bs) :< b

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap _ REmpty = REmpty
  fmap f (xs:<x) = (fmap f xs) :< f x

instance Applicative ReverseList where
  pure = (REmpty :<)
  REmpty <*> _ = REmpty
  (fs:<f) <*> xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
  REmpty >>= _ = REmpty
  (xs:<x) >>= f = (xs >>= f) <> (f x)