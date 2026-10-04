data Direction = LR | RL

type Stack = [(Direction, String)]

render :: Direction -> String -> String
render LR s = s
render RL s = reverse s

go :: String -> Stack -> String
go ('>' : '>' : cs) stack = go cs ((LR, "") : stack)
go ('<' : '<' : cs) stack = go cs ((RL, "") : stack)
go (':' : ':' : cs) ((direction, contents) : (parentDirection, parentContents) : rest) = go cs ((parentDirection, parentContents ++ render direction contents) : rest)
go (c : cs) ((direction, contents) : rest) = go cs ((direction, contents ++ [c]) : rest)
go [] stack = finish stack

finish :: Stack -> String
finish [(direction, contents)] = render direction contents
finish ((direction, contents) : (parentDirection, parentContents) : rest) = finish ((parentDirection, parentContents ++ render direction contents) : rest)

convertBiString :: String -> String
convertBiString s = go s [(LR, "")]