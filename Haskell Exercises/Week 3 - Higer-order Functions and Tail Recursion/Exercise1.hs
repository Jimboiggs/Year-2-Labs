curry :: ((a, a) -> a) -> (a -> a -> a)
curry f x y = f (x, y)

uncurry :: (a -> a -> a) -> ((a, a) -> a)
uncurry f (x, y) = f x y