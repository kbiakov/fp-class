import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS
import System.Random

operation :: (AbstractSet a) => a -> [Int] -> a
operation set list = foldl (add) set list

test1 :: (AbstractSet a) => a -> Bool
test1 s = contains (add (add (add s 3) 7) 6) 7

test2 :: (AbstractSet a) => a -> [Int] -> Bool
test2 s list = contains set (head list) && contains set (head $ tail list)
  where set = operation s list

test3 :: (AbstractSet a) => a -> Bool
test3 s = not $ contains (remove (add (add (add s 2) 2) 8) 2) 2

main = do
  print $ test1 (empty :: LS.Set) && test1 (empty :: TS.Set)
  gen <- newStdGen
  list <- take 10 $ randomRs (1, 100) gen
  print $ test2 (empty :: LS.Set) list && test2 (empty :: TS.Set) list
  print $ test3 (empty :: LS.Set) && test3 (empty :: TS.Set)
  
