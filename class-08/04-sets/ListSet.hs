module ListSet (Set, empty, isEmpty, add, contains, remove) where

import AbstractSet
import Data.List

newtype Set = SetImpl [Int]

instance AbstractSet Set where
  empty = SetImpl []
  
  isEmpty (SetImpl xs) = null xs
  
  add (SetImpl xs) x
    | elem x xs = SetImpl xs
    | otherwise = SetImpl (x : xs)
	
  contains (SetImpl xs) x = elem x xs

  remove (SetImpl xs) x = SetImpl (delete x xs)
