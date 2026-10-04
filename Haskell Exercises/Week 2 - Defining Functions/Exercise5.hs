import Data.Char

enc :: Int -> String -> String
enc n [] = ""
enc n (x:xs) = chr (ord x + n) : enc n xs

encrypt :: Int -> String -> (String, String -> String)
encrypt n str =
    let
        enc' :: Int -> String -> String
        enc' n [] = ""
        enc' n (x:xs) = chr (ord x + n) : enc' n xs
    in
        (enc' n str, enc' (-n))