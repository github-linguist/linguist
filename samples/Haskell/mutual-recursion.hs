f 0 = 1
f n | n > 0 = n - m (f $ n-1)

m 0 = 0
m n | n > 0 = n - f (m $ n-1)

main = do
       print $ map f [0..19]
       print $ map m [0..19]
