{-
4. Пользуясь средствами монады ST, запрограммировать сортировку массива тремя любыми методами.
-}

import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray

swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = elems $ runSTArray $ do
  let n = length xs
  arr <- newListArray (0, n-1) xs
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-i-2] $ \j -> do
      ai <- readArray arr j
      aj <- readArray arr (j+1)
      when (ai > aj) (swapElems j (j+1) arr)
  return arr

selectionSort :: (Ord a) => [a] -> [a]
selectionSort xs = elems $ runSTArray $ do
  let n = length xs
  arr <- newListArray (0, n-1) xs
  forM_ [0..n-2] $ \i ->
    forM_ [i+1..n-1] $ \j -> do
      x1 <- readArray arr i
      x2 <- readArray arr j
      when (x1 > x2) (swapElems i j arr)
  return arr

bubbleSortTest = bubbleSort [1, 6, 2, 5, 3, 4] == [1, 2, 3, 4, 5, 6]
selectionSortTest = selectionSort [1, 6, 2, 5, 3, 4] == [1, 2, 3, 4, 5, 6]
