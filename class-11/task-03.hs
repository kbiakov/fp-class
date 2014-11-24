{-
3. Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
-}

import Control.Monad
import Control.Monad.State

enqueue :: Int -> State [Int] ()
enqueue x = get >>= \xs -> put $ xs ++ [x]

dequeue :: State [Int] Int
dequeue = get >>= \(x:xs) -> put xs >> return x

testQuery :: State [Int] Int
testQuery = enqueue 1 >> enqueue 2 >> dequeue >> enqueue 2 >> dequeue
	
main = print $ runState testQuery [1, 2, 3, 4, 5]
