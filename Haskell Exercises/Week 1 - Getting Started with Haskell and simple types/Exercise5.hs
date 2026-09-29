data BinTree = Leaf a | Node a BinTree BinTree

numberOfLeaves :: BinaryTree a -> Int
numberOfLeaves(leaf x) = 1
numberOfLeaves(Node x left right) = numberOfLeaves left + numberOfLeaves right