module Assignment4 where

data BST a = EmptyNode | Node a (BST a) (BST a) deriving Show

createBST :: Ord a => [a] -> BST a
createBST [] = EmptyNode
createBST (x:xs) = Node x (createBST [a | a <- xs, a <= x]) (createBST [a | a <- xs, a > x])

toList :: BST a -> [a]
toList EmptyNode = []
toList (Node val lst rst) = [val] ++ toList lst ++ toList rst
  
isBST :: Ord a => BST a -> Bool
isBST EmptyNode = True
isBST (Node _val EmptyNode EmptyNode) = True
isBST (Node val EmptyNode rightSubTree@(Node r _ _)) = val < r && isBST rightSubTree
isBST (Node val leftSubTree@(Node l _ _) EmptyNode) = l < val && isBST leftSubTree
isBST (Node val leftSubTree@(Node l _ _) rightSubTree@(Node r _ _)) = l < val && val < r && isBST leftSubTree && isBST rightSubTree

depth :: BST a -> Int
depth EmptyNode = 0
depth (Node _ EmptyNode EmptyNode) = 1
depth (Node _ leftSubTree rightSubTree) = 1 + max (depth leftSubTree) (depth rightSubTree)

isBalanced :: BST a -> Bool
isBalanced EmptyNode = True
isBalanced (Node _ lst rst) = (2 >) $ (depth lst) - (depth rst)

attachSubTree :: Ord a => BST a -> BST a -> BST a
attachSubTree EmptyNode subTree = subTree
attachSubTree node EmptyNode = node
attachSubTree (Node val lst rst) subTree@(Node x _ _)
  | x <= val = Node val (attachSubTree lst subTree) rst
  | otherwise = Node val lst (attachSubTree rst subTree)
  
leftRotate :: Ord a => BST a -> BST a
leftRotate EmptyNode = EmptyNode
leftRotate (Node val lst rst) = attachSubTree rst (Node val lst EmptyNode)

rightRotate :: Ord a => BST a -> BST a
rightRotate EmptyNode = EmptyNode
rightRotate (Node val lst rst) = attachSubTree lst (Node val EmptyNode rst)

balanceHelper :: BST a -> BST a -> BST a
balanceHelper EmptyNode _parent = EmptyNode
balanceHelper (Node _val _lst _rst) parent = parent

balance :: Ord a => BST a -> BST a
balance EmptyNode = EmptyNode
balance tree@(Node _ EmptyNode EmptyNode) = tree
balance tree@(Node v EmptyNode rst) =
  if (depth rst) > 2 then
    leftRotate (Node v EmptyNode (balance rst))
  else
    tree
balance tree@(Node v lst EmptyNode) =
  if (depth lst) > 2 then
    rightRotate (Node v (balance lst) EmptyNode)
  else
    tree
balance tree@(Node v lst rst) =
  if (depth lst) - (depth rst) > 2 then
    rightRotate (Node v (balance lst) (balance rst))
  else if (depth lst) - (depth rst) < -2 then
    leftRotate (Node v (balance lst) (balance rst))
  else
    tree

instance Functor BST where
--  fmap :: (Ord a, Ord b) => (a -> b) -> BST a -> BST b
  fmap _ EmptyNode = EmptyNode
  fmap fn (Node val lst rst) = createBST $ toList $ Node (fn val) (fmap fn lst) (fmap fn rst)

