{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

import Data.Char
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import System.Environment

incKey key map = case (Map.lookup key map) of
  Just x -> Map.update (\value -> Just (value + 1)) key map
  Nothing -> Map.insert key 0 map

freqPunct content = Map.keys $ Map.filter (== max) map0
  where
    map0 = foldl' (\acc x -> 
	    | isPunctuation x = (incKey x acc)
	    | otherwise = acc)
	  Map.empty content
    max = maximum $ Map.elems map0

freqWords content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
  where
    list = filter (not . null) $ map (map toLower . filter isLetter) $ words content
    map0 = foldl' (\acc word -> incKey word acc) Map.empty list
	
freqChars content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
  where
    map0 = foldl' (\acc x -> 
	    | x == ' ' = acc 
	    | otherwise = incKey x acc)
	  Map.empty content
	
freqBigrs content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
  where
    map0 = fst $ foldl' (\(acc, prev) x ->
	    | x == ' ' || prev == ' ' = acc
	    | otherwise = incKey [ prev : x : [] ] acc, x)
	  (Map.empty, ' ') content	

freqTrigs content = take 10 $ sortBy (flip compare `on` snd) $ Map.toList map0
  where
    list = filter (\x -> isLetter x || x == ' ') content
    (map0, _, _) = foldl' (\(acc, p1, p0) x ->
	    | x == ' ' || p0 == ' ' || p1 == ' ' = acc
	    | otherwise = incKey [ p1 : p0 : x : [] ] acc, p0, x)
	  (Map.empty, ' ', ' ') list
	
main = do
  (fname : _) <- getArgs
  content <- readFile fname
  putStrLn ("1) Frequent punctuation marks: " ++ (show $ freqPunct content))
  putStrLn ("2) 50 frequent words: " ++ (show $ freqWords content))
  putStrLn ("3) Frequent symbols: "  ++ (show $ freqChars content))
  putStrLn ("3) Frequent digrams: "  ++ (show $ freqBigrs content))
  putStrLn ("3) Frequent trigrams: " ++ (show $ freqTrigs content))
