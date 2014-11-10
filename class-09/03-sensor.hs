import System.Environment
import Data.Monoid

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData = map (\x -> if x == "-" then Nothing else Just (read x :: Int)) . lines

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay xs = (take 5 xs) : (dataByDay $ drop 5 xs)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 flag = fromJust . minimum . filter isJust . map ans . filter $ any isJust
  where ans
    | flag == True = getFirst . mconcat . map First
    | flag == False = getLast . mconcat . map Last

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 flag = minimum . map ans . filter $ any isJust
  where ans
    | flag == True = getSum . mconcat . map (Sum . fromJust) . filter isJust
    | otherwise = getProduct . mconcat . map (Product . fromJust) . filter isJust

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct
  deriving Eq

minData :: SensorTask -> [SensorData] -> Int
minData state
  | state == NeedFirst   = minData1 True
  | state == NeedLast    = minData1 False
  | state == NeedSum     = minData2 True
  | state == NeedProduct = minData2 False

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

forPred :: [[Maybe a]] -> [[Bool]]
forPred = map (map (not . isNothing ))

stat :: [Int] -> [SensorData] -> Int
stat args data' = let n : number = args in 
  case n of
    1 -> length . filter (== False) $ map (getAny . mconcat . map Any) $ forPred data'
    2 -> length . filter (== True) $ map (getAll . mconcat. map All) $ forPred data'
    3 -> length . filter (== True) $ map (getAny . mconcat. map Any) $ forPred data'
    4 -> length . filter (> head number) $ map (getSum. fromJust. mconcat ) $ forSum data' 
    5 -> length . filter (> head number) $ map (getProduct. fromJust. mconcat ) $ forProduct data' 
    6 -> length . filter (\x -> (not . isNothing $ x) && fromJust x > head number) $ map (getFirst. mconcat . map First) data' 
    7 -> length . filter (\x -> (not . isNothing $ x) && fromJust x > head number) $ map (getLast. mconcat . map Last) data' 

main = do
  fname:args <- getArgs
  sData <- getData `fmap` readFile fname
  let n = if length args == 1 then [read. head $ args] else [read. head $ args, read. last $ args]
  let data' = dataByDay sData
  print $ minData NeedFirst data'
  print $ minData NeedLast data'
  print $ minData NeedSum data'
  print $ minData NeedProduct data'  
  print $ stat n data'
  
