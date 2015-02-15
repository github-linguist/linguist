bsd n = r:bsd r where r = ((n * 1103515245 + 12345) `rem` 2^31)
msr n = (r `div` 2^16):msr r where r = (214013 * n + 2531011) `rem` 2^31

main = do
	print $ take 10 $ bsd 0 -- can take seeds other than 0, of course
	print $ take 10 $ msr 0
