import Data.List

stripComments :: String -> String -> String -> String
stripComments start end = notComment
    where notComment :: String -> String
          notComment "" = ""
          notComment xs
            | start `isPrefixOf` xs = inComment $ drop (length start) xs
            | otherwise             = head xs:(notComment $ tail xs)
          inComment :: String -> String
          inComment "" = ""
          inComment xs
            | end `isPrefixOf` xs = notComment $ drop (length end) xs
            | otherwise           = inComment $ tail xs

main = interact (stripComments "/*" "*/")
