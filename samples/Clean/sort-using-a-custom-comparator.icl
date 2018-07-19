import StdEnv

less s1 s2
    | size s1 > size s2 = True
    | size s1 < size s2 = False
    | otherwise = lower s1 < lower s2
where
    lower :: String -> String
    lower s = {toLower c \\ c <-: s}

Start = sortBy less ["This", "is", "a", "set", "of", "strings", "to", "sort"]
