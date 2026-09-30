data List a = Empty | Cons a (List a)

len :: List a -> Int
len Empty = 0
len [a] = 1
len (Cons x tail) = 1 + len tail