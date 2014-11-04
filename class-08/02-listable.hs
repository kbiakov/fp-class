{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
  toList :: a -> [a]
  fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
  toList str = words str
  fromList str = unwords str
  
instance Listable Integer where
  toList n = reverse $ tmp n
  where
    tmp 0 = []
    tmp n = (n `mod` 10) : (tmp (n `div` 10))
  fromList = foldl (\acc x -> acc * 10 + x) 0
