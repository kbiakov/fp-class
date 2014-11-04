import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS
import System.Random

test1 :: (AbstractSet a) => a -> Bool
test1 s = contains (add (add (add s 3) 7) 6) 7

test2 :: (AbstractSet a) => a -> Bool
test2 s = not $ contains (remove (add (add (add s 2) 2) 8) 2) 2

test3 :: (AbstractSet a) => a -> [Int] -> Bool
test3 s list = contains set (head list) && contains set (head $ tail list)
  where set = foldl (add) s list

main = do
  gen <- newStdGen
  let list = take 10 $ randomRs (1, 100) gen
  print $ test1 (empty :: LS.Set) && test1 (empty :: TS.Set)
  print $ test2 (empty :: LS.Set) && test2 (empty :: TS.Set)
  print $ test3 (empty :: LS.Set) list && test3 (empty :: TS.Set) list
