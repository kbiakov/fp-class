import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
  | a `mod` 3 == 0 = 0
  | odd a = a * a
  | otherwise = a ^ 3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 a = a
reduceNF n a = reduceNF (n - 1) (fmap reduce a)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList [] = []
toList xs = map (\(x, y) -> x * x - 2 * x * y + y * y) xs

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs =  fmap (sum . map (\(x, y) -> x * x - 2 * x * y + y * y)) (Just xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty!"
toEither xs = fmap (sum . map (\(x, y) -> x * x - 2 * x * y + y * y)) (Right xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: [(Integer, Integer)] -> IO Integer
toIO xs = do
  (num, _) <- fmap (randomR (1, 100)) newStdGen
  return $ (sum (map (\(x, y) -> x * y) xs)) `mod` num

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs ls = (head ls, read $ last ls)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
  content <- readFile fname
  return $ map (\s -> let [f', s'] = words s in (read f', read s')) $ lines content

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}

{-
  02-test.txt, n = 2
  
  [1,214358881,1,966407804840271334616088784666624,0]
  Just 1245020818536021435729923829661696
  Right 1245020818536021435729923829661696
  625
-}
