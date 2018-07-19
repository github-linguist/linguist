stripChars :: String -> String -> String
stripChars = filter . flip notElem
