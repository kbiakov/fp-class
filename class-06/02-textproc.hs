{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
  регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
  соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
  максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.IO
import System.Environment
import System.Directory
import System.Random
import Data.List
import Data.Char(toUpper)

task :: Int -> [String] -> IO ()
task 1 = countLines
task 2 = appendStr
task 3 = toUpper'
task 4 = zipFile
task 5 = rndFile

main = do
  (num:args) <- getArgs
  task (read num) args

-- 1
countLines [fname] = do
  contents <- readFile fname
  print $ length . filter (== '\n') $ contents

-- 2
appendStr [fname, str] = do
  appendFile fname $ '\n' : str

-- 3
toUpper' [fname] = do
  contents <- readFile fname
  putStr . unlines . map (map toUpper) $ lines contents

-- 4
zipStr :: String -> String -> String
zipStr str1 str2 = unlines $ zipWith (++) (lines str1) (lines str2)

zipFile [f1, f2] = do
  put1 <- readFile f1
  put2 <- readFile f2
  putStr $ zipStr put1 put2

-- 5
rndFile [m, n, fname] = do
  gen <- getStdGen
  sequence_ $ take (rndN (read m) gen) $ repeat g
  where
    g :: IO ()
    g = do
      nGen <- newStdGen
      appendFile fname $ ((take (rndN (read n) nGen) $ rndS nGen) ++ "\n")
      rndS gen = randomRs ('a', 'z') gen :: [Char]
      rndN n gen = fst $ randomR (1, n) gen :: Int
