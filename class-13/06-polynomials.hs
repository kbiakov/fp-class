{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import FloatParser
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Ord
import Data.List

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}

type Factor = Float

type Power = Int

data Summand = Summand (Factor, Power)
	deriving Show
	
data Op = Plus | Minus | Mul | Div
	deriving Show
	
data Poly = Poly [Summand]
	deriving Show
	
{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

sign :: Parser Float
sign = (char '-' >> return (-1.0)) <|> (char '+' >> return 1.0)

factor :: Parser Float
factor = float <|> ((*) <$> sign <*> token (optional 1 float))

power :: Parser Int
power = char 'x' >> (optional 1 (char '^' >> integer))

summand = Summand `liftM` token (wFact <|> woFact)
  where
    wFact  = (,) <$> factor <*> optional 0 power
    woFact = (,) <$> return 1 <*> power
		
poly :: Parser Poly
poly = Poly `liftM` (many summand)

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
divmod :: Poly -> Poly -> (Poly, Poly)
divmod = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = undefined

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = undefined
