module Logic3 where

{-
  1. Определить тип Logic3 c тремя значениями: T (истина), U (неизвестно) и F (ложь),
  добавив порождение необходимых экземпляров стандартных классов типов. Следует иметь
  в виду, что наличие экземпляров некоторых классов типов может значительно облегчить
  реализацию требуемых функций.
-}

data Logic3 = T -- Истина
            | U -- Неизвестно
            | F -- Ложь
            deriving(Eq, Show)

{-
  2. Реализовать логическую операцию not3, определяемую таблицей:

  A | not3 A
 -----------
  T | U
  U | F
  F | T
  
  Замечание. Отрицание, как и любую другую функцию трёхзначной логики, обобщающую соответствующую
  функцию двоичной логики, можно определить разными способами. Ясно, что свойства таких функций
  будут различаться. В данном определении было обобщено свойство «циклического сдвига», в другом
  варианте — отрицании Лукасевича — отрицание истины есть ложь, отрицание лжи есть истина,
  отрицание неизвестности есть неизвестность (это обобщение свойства «симметричности» двоичного
  отрицания). Подробнее об этом можно почитать в книге С. В. Яблонского «Введение в дискретную
  математику» (глава 2).
-}

not3 :: Logic3 -> Logic3
not3 T = U
not3 U = F
not3 F = T 

{-
  3. Реализовать логические операции \/ (дизъюнкция) и /\ (конъюнкция), определяемые следующими
  таблицами (первый столбец каждой таблицы соответствует левому аргументу операции, а первая
  строка — правому аргументу):

  /\ |  T  U  F         \/ |  T  U  F
  -------------         ------------- 
  T  |  T  U  F         T  |  T  T  T
  U  |  U  U  F         U  |  T  U  U
  F  |  F  F  F         F  |  T  U  F

-}

(\/) :: Logic3 -> Logic3 -> Logic3
T \/ _ = T
_ \/ T = T
U \/ _ = U
_ \/ U = U
F \/ F = F

(/\) :: Logic3 -> Logic3 -> Logic3
F /\ _ = F
_ /\ F = F
U /\ _ = U
_ /\ U = U
T /\ T = T

-- 4. Реализовать аналоги стандартных функций and, or, any, all для случая трёхзначной логики.

and3, or3 :: [Logic3] -> Logic3
and3 = foldl1 (/\)
or3  = foldl1 (\/)

any3, all3 :: (a -> Logic3) -> [a] -> Logic3
any3 pred = foldl (\/) F . map pred
all3 pred = foldl (/\) T . map pred

{-
  5. Перебирая все возможные значения логической переменной, доказать тождественную истинность
  следующей формулы (закон исключённого четвёртого): x \/ not3 x \/ not3 (not3 x).
-}

excluded_fourth :: Logic3
excluded_fourth = all3 (/\T) $ map (\x -> x \/ not3 x \/ not3 (not3 x)) [T, U, F]

-- Должно быть True
test_excluded_fourth = excluded_fourth == T
