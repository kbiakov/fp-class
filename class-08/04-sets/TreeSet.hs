module TreeSet (Set, empty, isEmpty, add, contains, remove) where

import AbstractSet
import BST

newtype Set = SetImpl (Tree Int)

instance AbstractSet Set where
  empty = SetImpl EmptyTree
  
  isEmpty (SetImpl EmptyTree) = True
  isEmpty (SetImpl _) = False
  
  add (SetImpl t) x = SetImpl (insertTree x t)
  
  contains (SetImpl t) x = containsTree x t
  
  remove (SetImpl t) x = SetImpl (deleteTree x t)
