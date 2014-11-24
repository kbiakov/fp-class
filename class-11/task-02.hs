{-
2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad
import Control.Monad.Writer
import System.Environment

taylorSeries :: Double -> Double -> Double -> Double -> Writer [Double] Double
taylorSeries prevVal prevSum n x = tell [prevSum] >> 
  if (abs (prevSum - nextSum) < 1e-10) then return prevVal 
  else taylorSeries (prevVal + nextSum) nextSum (n + 2) x 
  where nextSum = - prevSum * x * x/(n + 1)/(n + 2)

mySin :: Double -> (Double, [Double])
mySin x = runWriter $ taylorSeries x x 1 x

myCos :: Double -> (Double, [Double])
myCos x = runWriter $ taylorSeries 1 1 0 x
