import Data.List (sort)

primes = 2:filter isPrime [3,5..] where
    isPrime n = f n primes where
        f n (p:ps)
            | p*p > n = True
            | n`mod`p == 0 = False
            | otherwise = f n ps

primeFactors n = f n primes where
    f n (p:ps)
        | p*p > n = if n == 1 then [] else [(n,1)]
        | n`mod`p == 0 = (p,e):f res ps
        | otherwise = f n ps
        where (e,res) = ppower (n`div`p) p 1
    ppower n p e
        | n`mod`p /= 0 = (e,n)
        | otherwise = ppower (n`div`p) p (e+1)

factors n = comb (primeFactors n) where
    comb [] = [1]
    comb ((p,e):others) = [a*b | b<-map (p^) [0 .. e], a<-comb others]

ndigit 0 = 0
ndigit n = 1 + ndigit (n`div`10)

fangs n
    | odd w = []
    | otherwise = map (\x->(x,n`div`x)) $ filter isfang (factors n) where
        isfang x = x > xmin && x < y && y < ymax &&      -- same length
            ((x`div`10)/=0 || (y`div`10)/=0) &&          -- not zero-ended
            sort (show n) == sort ((show x) ++ (show y)) -- same digits
            where y = n`div`x
        w = ndigit n
        xmin = 10^(w`div`2-1)
        ymax = 10^(w`div`2)

vampires = filter ((0<).length.fangs) [1..]

main = mapM (\n->print (n,fangs n)) $
        ((take 25 vampires) ++ [16758243290880, 24959017348650, 14593825548650])
