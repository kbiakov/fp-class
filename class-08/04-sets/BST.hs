module BST where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = Node x EmptyTree EmptyTree
insertTree x (Node a left right)
  | x == a = Node a left right
  | x < a = Node a (insertTree x left) right
  | x > a = Node a left (insertTree x right)

containsTree :: (Ord a) => a -> Tree a -> Bool
containsTree _ EmptyTree = False
containsTree x (Node a left right)
  | x == a = True
  | x < a = containsTree x left
  | x > a = containsTree x right

deleteTree :: (Ord a) => a -> Tree a -> Tree a
deleteTree _ EmptyTree = EmptyTree
deleteTree x (Node a left right)
  | x == a = deleteNode (Node a left right)
  | x < a = deleteTree x left
  | x > a = deleteTree x right
  where
    deleteNode (Node _ EmptyTree EmptyTree) = EmptyTree
    deleteNode (Node _ EmptyTree right) = right
    deleteNode (Node _ left EmptyTree) = left
    deleteNode (Node _ left right) = Node root left right'
    where
      minKey (Node x EmptyTree _) = x
      minKey (Node _ l _) = minKey l
      root = minKey right
      right' = deleteTree root right
