module Tree (insert) where

data BinaryTree a = Empty | Leaf a | Node (BinaryTree a) a (BinaryTree a)

insert :: Ord a => BinaryTree a -> a -> BinaryTree a
insert (Empty) y = Leaf y
insert (Leaf x) y | x > y = Node (Leaf y) x Empty
                  | x <= y = Node Empty x (Leaf y)
insert (Node l x r) y | x <= y = Node l x (insert r y)
                      | x > y = Node (insert l y) x r

inOrder :: Show a => BinaryTree a -> String
inOrder Empty = "Empty"
inOrder (Leaf x) = show x
inOrder (Node l x r) = (inOrder l) <> show x <> (inOrder r)
