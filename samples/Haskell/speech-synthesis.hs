import System

say x = system $ "espeak " ++ show x

main = say "This is an example of speech synthesis."
