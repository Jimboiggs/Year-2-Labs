data Prop = Var Char | And Prop Prop | Or Prop Prop | Implies Prop Prop | Neg Prop

instance Show Prop where
    show (Var x) = [x]
    show (And p q) = "(" ++ show p ++ " && " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " || " ++ show q ++ ")"
    show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show (Neg p) = "(!" ++ show p ++ ")"