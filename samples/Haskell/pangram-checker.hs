import Data.Char (toLower)
import Data.List ((\\))

pangram :: String -> Bool
pangram = null . (['a' .. 'z'] \\) . map toLower

main = print $ pangram "How razorback-jumping frogs can level six piqued gymnasts!"
