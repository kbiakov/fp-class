import Control.Monad
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Maybe Pole
updatePole p = if unbalanced p then imbalanced p else Right p
  where
    unbalanced (l, r) = abs (l - r) >= balance
	imbalanced (l, r)
	  | l > r = Left "Imbalanced right"
	  | l < r = Left "Imbalanced left"

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) = updatePole (left, right + n)

banana :: Pole -> Either String Pole
banana = const (Left "Banana")

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = Right (left + n, right + n)

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0, 0))



interpretate :: String -> Pole -> Either String Pole
interpretate s = 
  let bs = read . tail . dropWhile (/= ' ')
  in case head s of
    'L' -> landLeft  (bs s)
    'R' -> landRight (bs s)
	  'B' -> banana
    'O' -> landBoth  (bs s)
    'U' -> unlandAll

readF :: FilePath -> IO [Pole -> Either String Pole]
readF fname = readFile fname >>= return . map interpretate . lines  

testF :: Pole -> [Pole -> Either String Pole] -> Either String Pole
testF x0 s = return x0 >>= \x -> foldl (>>=) (head s $ x) (tail s)



tests = all test [1..4]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "Imbalanced left"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Banana"
    test 4 = (return (0, 0) >>= landRight 2 >>= landLeft 3 >>= landRight -1 >>= banana >>= landLeft 1) == Left "Banana"

main = print tests

-- readF "1.txt" >>= print . testF (0,0)  
