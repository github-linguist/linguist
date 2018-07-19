gray_encode(n) = n $ (n >> 1)

function gray_decode(n)
    p = n
    while (n >>= 1) != 0
        p $= n
    end
    return p
end
