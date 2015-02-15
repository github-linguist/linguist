import Data.List

-- Return the common prefix of two lists.
commonPrefix2 (x:xs) (y:ys) | x == y = x : commonPrefix2 xs ys
commonPrefix2 _ _ = []

-- Return the common prefix of zero or more lists.
commonPrefix (xs:xss) = foldr commonPrefix2 xs xss
commonPrefix _ = []

-- Split a string into path components.
splitPath = groupBy (\_ c -> c /= '/')

-- Return the common prefix of zero or more paths.
-- Note that '/' by itself is not considered a path component,
-- so "/" and "/foo" are treated as having nothing in common.
commonDirPath = concat . commonPrefix . map splitPath

main = putStrLn $ commonDirPath [
        "/home/user1/tmp/coverage/test",
        "/home/user1/tmp/covert/operator",
        "/home/user1/tmp/coven/members"
       ]
