function Binomial( n, k )
    if k > n then return nil end
    if k > n/2 then k = n - k end       --   (n k) = (n n-k)

    numer, denom = 1, 1
    for i = 1, k do
        numer = numer * ( n - i + 1 )
        denom = denom * i
    end
    return numer / denom
end
