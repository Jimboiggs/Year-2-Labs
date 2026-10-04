fourth :: [a] -> a
fourth xs = head (tail (tail (tail xs)))

fourth :: [a] -> a
fourth xs = xs !! 3

fourth :: [a] -> a
fourth (_:_:_:x:_) = x