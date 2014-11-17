{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import System.Environment
import Control.Monad
import Data.Monoid
import Data.List
import Data.Ord

data Student = Student {
  name :: String,
  age :: Int,
  group :: String
} deriving (Eq)

instance Show Student where
  show (Student n a g) = n ++ " " ++ (show a) ++ " " ++ g

instance Ord Student where
  s1 `compare` s2 = (name s1) `compare` (name s2)
  s1 <= s2 = (name s1) <= (name s2)
  
group3 :: [a] -> [[a]]
group3 [] = []
group3 ls = [take 3 ls] ++ (group3 $ drop 3 ls)

readF :: FilePath -> IO [Student]
readF fname = readFile fname >>= return . (foldr (\x acc -> impStudent x:acc) []) . group3 . lines
  where impStudent [n, a, g] = Student n (read a) g

writeF :: FilePath -> [Student] -> IO ()
writeF fname list = writeFile fname $ unlines $ map show list

main = (++) `liftM` readF "1.txt" `ap` readF "2.txt" >>= writeF "12.txt" . sortBy (comparing name)
