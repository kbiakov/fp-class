{-
  Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
  на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.
  
  Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
  (для ghc и ghci), например:
  
  $ ghc 05-graham.hs -o graham -i../class-05/3-Graham_scan/
-}

import GrahamScan
import System.Environment

readPts :: String -> [Point]
readPts str = foldr readLine [] $ lines str
  where
    readLine ln acc =
      let (x : y : _) = map read $ words ln :: [Double]
      in (Point x y : acc)
	  
writePts :: [Point] -> String
writePts = foldr addPoint ""
  where
    addPoint (Point x y) acc = (show x) ++ " " ++ (show y) ++ "\n" ++ acc
		
main = do
  [input, output] <- getArgs
  content <- readFile input
  let
    pts = graham_scan $ readPts content
    writeFile output $ writePts pts
    return ()
	
