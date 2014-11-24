{-
1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}

import System.Environment
import Control.Monad.Reader

readConfig :: String -> Integer -> Integer
readConfig str =
  let (oper, oth) = span (/= '=') str
      arg = (\x -> read x :: Integer) . tail $ oth
  in case oper of
    "summand" -> (+ arg)
    "multiplier" -> (* arg)
    "divisor" -> (`div` arg)

evaluate :: Integer -> Reader [Integer -> Integer] Integer
evaluate num = do
  cfg <- ask
  return $ foldl (flip ($)) num cfg 

main = do
  [fnConf, fnNums] <- getArgs
  nums <- (map (\x -> read x :: Integer) . lines) <$> readFile fnNums
  conf <- (map readConfig . lines) <$> readFile fnConf
  print $ map (\x -> (runReader $ evaluate x) conf) nums
