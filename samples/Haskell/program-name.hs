import System (getProgName)

main :: IO ()
main = getProgName >>= putStrLn . ("Program: " ++)
