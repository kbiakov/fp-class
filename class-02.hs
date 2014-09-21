-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms time = (h, m, s)
  where h =  time `div` 3600
        m = (time `mod` 3600) `div` 60
		    s =  time `mod` 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h , m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

triangle :: (Point, Point, Point) -> (Double, Double)
triangle (x, y, z) = (p, s)
  where a = distance x y
        b = distance x z
        c = distance y z
        p = a + b + c
        s = sqrt (p/2 * (p/2 - a) * (p/2 - b) * (p/2 - c))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
  | even x = 1 + nEven xs
  | otherwise nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems (x:xs) = x * 2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
  | odd x = x : fltOdd xs
  | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
nonNeg :: Integral a => [a] -> [a]
nonNeg [] = []
nonNeg (x:xs)
  | x < 0 = nonNeg xs
  | otherwise = x : nonNeg xs
  
-- б) увеличить элементы с чётными значениями в два раза;
twEven :: Integral a => [a] -> [a]
twEven [] = []
twEven (x:xs)
  | even x = 2 * x : twEven xs
  | otherwise  = x : twEven xs
  
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
swapEO :: Integral a => [a] -> [a]
swapEO [] = []
swapEO [x] = []
swapEO (x:y:xs) = y : x : swapEO xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x + y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: [Integer] -> [Integer] -> [(Integer, Integer)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x, y) : combine_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstNatA :: Integer -> [Integer]
firstNatA 0 = []
firstNatA n = n : firstNatA (n - 1)

-- б) в порядке возрастания.
firstNatB :: Integer -> [Integer]
firstNatB 0 = []
firstNatB n = firstNatB (n - 1) ++ [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insBetween :: a -> [a] -> [a]
insBetween e [] = []
insBetween e [x] = [x]
insBetween e (x:xs) = x : e : insBetween e xs 

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
divList :: Eq a => [a] -> ([a], [a])
divList [] = ([], [])
divList (x:xs) = divListIt [x] xs
  where divListIt xs [] = (xs, [])
        divListIt xs (y:ys)
          | x == y = divListIt (x:xs) ys
          | otherwise = (xs, y:ys)

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):

-- а) [a] -> Int -> a
getAt :: Num a => [a] -> Int -> a
getAt [] n = error "Invalid n"
getAt (x:xs) 0 = x
getAt (x:xs) n = getAt xs (n - 1)

-- б) Eq a => [a] -> a -> Bool
isExist :: Eq a => [a] -> a -> Bool
isExist [] y = False
isExist (x:xs) y
  | x == y = True
  | otherwise = isExist xs y

-- в) [a] -> Int -> [a]
delAt :: [a] -> Int -> [a]
delAt [] _ = []
delAt (x:xs) 0 = xs
delAt (x:xs) n = x : delAt xs (n - 1)

-- г) a -> Int -> [a]
repN :: a -> Int -> [a]
repN _ 0 = []
repN x n = x : repN x (n - 1)

-- д) [a] -> [a] -> [a]
ccat :: [a] -> [a] -> [a]
ccat xs [] = xs
ccat [] ys = ys
ccat (x:xs) ys = x : ccat xs ys

-- е) Eq a => [a] -> [[a]]
lols :: Eq a => [a] -> [[a]]
lols [] = []
lols (x:y:xs)
  | x == y = (y:xs) : lols (y:xs)
  | otherwise = lols xs

-- ж) [a] -> [(Int, a)]
indxs :: [a] -> [(Int, a)]
indxs xs = indxs' xs 0
  where indxs' [] _ = []
        indxs' (x:xs) n = (n, x) : indxs' xs (n + 1)

-- з) Eq a => [a] -> [a]
delTw :: Eq a => [a] -> [a]
delTw [] = []
delTw (x:xs)
  | xs `contains` x = delTw xs
  | otherwise = x : delTw xs
