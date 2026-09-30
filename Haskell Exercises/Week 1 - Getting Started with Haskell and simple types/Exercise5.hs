data BinaryTree a = Leaf a | Node a (BinaryTree a) (BinaryTree a)

numberOfLeaves :: BinaryTree a -> Int
numberOfLeaves(Leaf x) = 1
numberOfLeaves(Node x left right) = numberOfLeaves left + numberOfLeaves right