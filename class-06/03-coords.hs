{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
  (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
  из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.IO
import System.Environment
import System.Directory

task :: (Eq a, Fractional a) => a -> [String] -> IO ()
task 1 = startRandomizeWithBounds
task 2 = findAlloc
task 3 = findDistant

main = do
  (num:args) <- getArgs
  task (read num) args

-- 1
type Bounds = (Double, Double, Double, Double)

randomPoint :: Bounds -> IO String
randomPoint (xMin, xMax, yMin, yMax) = do
  g <- newStdGen
  let
    (x, newGen) = randomR (xMin, xMax) g :: (Double, StdGen)
    y = fst $ randomR (yMin, yMax) newGen :: Double
  return $ unwords [show x, show y]

randomFile :: (String, Int, Bounds) -> IO ()
randomFile (fname, nLns, bnds) = do
  if nLns == 0 then return () else (do
    newStr <- randomPoint bnds
    appendFile fname $ newStr ++ "\n"
    randomFile (fname, nLns-1, bnds))

startRandomizeWithBounds args@[fname, nLns, xMin, xMax, yMin, yMax] = do
  let
    b = map read $ drop 2 args :: [Double]
    bnds@(xMin, xMax, yMin, yMax) = (b!!0, b!!1, b!!2, b!!3)
    writeFile fname ""
    randomFile (fname, read nLns, bnds)

-- 2
proccessPnt :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
proccessPnt (p1, p2, p3, p4) str
  | x > 0 && y > 0 = (p1 + 1, p2, p3, p4)
  | x < 0 && y > 0 = (p1, p2 + 1, p3, p4)
  | x < 0 && y < 0 = (p1, p2, p3 + 1, p4)
  | x > 0 && y < 0 = (p1, p2, p3, p4 + 1)
  | otherwise = (p1, p2, p3, p4)
  where (x:y:xs) = map read $ words str :: [Double]
	
findAlloc [fname] = do
  contents <- readFile fname
  let (p1, p2, p3, p4) = foldl proccessPnt (0, 0, 0, 0) $ lines contents
  putStr $ "1/4: " ++ (show p1) ++ "\n"
  putStr $ "2/4: " ++ (show p2) ++ "\n"
  putStr $ "3/4: " ++ (show p3) ++ "\n"
  putStr $ "4/4: " ++ (show p4) ++ "\n"
  
-- 3
findDistant [fname] = do
  contents <- readFile file fname
  putStrLn $ show $ foldl1 foldFunc $ map (\x -> read x :: (Int, Int)) $ lines contents
  where
    dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
    foldFunc acc x
	  | dist (0, 0) acc > dist (0, 0) x = x 
    | otherwise = acc
	  
