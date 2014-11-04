import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ
import System.Environment
import System.Random

push :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> q a
push q (x:xs) 0 = q
push q (x:xs) n = push (enqueue q x) xs (n - 1)

pop :: (AbstractQueue q, Num a, Eq a) => q a -> Int -> q a
pop q 0 = q
pop q n = pop (snd $ dequeue q) (n - 1)

next' :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> q a
next' q list n = pop (push q list n) (n - 1)

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> [a] -> Int -> Int -> q a
checkQueue q randList k n
  | k <= n = checkQueue (next' q randList k) (drop k randList) (k + 1) n
  | otherwise = q
  
queue2list q
  | isEmpty q = []
  | otherwise = [x] ++ (queue2list temp)
  where (x, temp) = dequeue q

main = do
  [params] <- getArgs
  gen <- newStdGen
  let n = read params :: Int
  let list = randomRs (1, 100) gen
  let q = checkQueue (empty :: Q.Queue Int) list 1 n
  let fq = checkQueue (empty :: FQ.Queue Int) list 1 n
  let sq = checkQueue (empty :: SQ.Queue Int) list 1 n
  print $ ((queue2list q) == (queue2list sq)) == ((queue2list fq) == (queue2list q))
