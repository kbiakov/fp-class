{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point { x :: Double, y :: Double }
deriving (Show, Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = L | S | R
deriving (Show, Eq)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

directions :: [Point] -> [Direction]
directions (a:b:c:rest) = [findDir a b c] ++ directions (b:c:rest)
directions _ = []
findDir a b c
  | res > 0 = L
  | res < 0 = R
  | otherwise = S
where
  res = ux * vy - uy * vx
  (ux, uy) = (x b - x a, y b - y a)
  (vx, vy) = (x c - x a, y c - y a)

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

graham_scan :: [Point] -> [Point]
graham_scan = delInv . sortLeft . getPoint
  where
    getPoint points = foldl func (head points, []) (tail points)
    where
	  func ((Point x y), others) (Point nx ny)
	    | nx < x    = ((Point nx ny), (Point x y):others)
	    | otherwise = ((Point x y), (Point nx ny):others)
	sortLeft (key, others) = (key, sortBy sortFunc others)
	where
	  sortFunc p pp
        | findDir key p pp == L = LT
        | findDir key p pp == S = if distance key p < distance key pp then LT else GT
        | otherwise = GT
    delInv (key, x:others) = (foldl delFunc (x:key:[]) others)
	where
	  delInv' (x:y:z:stack)
	    | findDir x z y == R = delInv' (x:z:stack)
	    | otherwise = (x:y:z:stack)
      delFunc acc p = delInv' (p:acc)

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_test1 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 2 0, Point 2 2]
graham_test2 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 2 0, Point 2 2, Point 3 5]
graham_test3 = graham_scan [Point 0 0, Point 1 1, Point 0 2, Point 4 0, Point 2 0, Point 2 2]
