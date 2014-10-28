{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Environment
import Data.List
import Data.Char
import System.IO
import System.Random

randomF :: [String] -> IO()
randomF args = do
  let [fname, from, to, charn, rown] = args
  mapM_ (\x -> x) $ replicate (read rown) $ do
    g <- newStdGen
    let numbers = take (read charn) $ randomRs (read from, read to) g ::[Int]
    appendFile fname $ foldl (\acc x -> show x ++ " " ++ acc ) "" numbers ++ "\n"
	
main = do
  args <- getArgs
  randomF args
  
