wellBracketed :: String -> Bool
wellBracketed xs =
    let (count, valid) = foldl (\(count, valid) c ->
        if c == '(' then (count + 1, valid)
        else if c == ')' then (count - 1, valid && count - 1 >= 0)
        else (count, valid)
    ) (0, True) xs
    in count == 0 && valid