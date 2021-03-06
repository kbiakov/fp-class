{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad
import Data.Foldable

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length . nub

nub_seq :: (Eq a) => Seq.Seq a -> Int
nub_seq s
  | Seq.fromList [] == s = 0
  | Data.Foldable.elem l sd = nub_seq sd
  | otherwise = (nub_seq sd) + 1
  where
    (l Seq.:< _) = Seq.viewl s
    sd = Seq.drop 1 s

nub_arr :: Array Int Int -> Int
nub_arr x = length $ nub $ Iarray.elems x

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
