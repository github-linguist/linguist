import Data.Char

main :: IO ()
main = do
	let hello = "hello world"
	putStrLn $ map toUpper hello