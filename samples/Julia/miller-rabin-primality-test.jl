witnesses(n::Union(Uint8,Int8,Uint16,Int16)) = (2,3)
witnesses(n::Union(Uint32,Int32)) = n < 1373653 ? (2,3) : (2,7,61)
witnesses(n::Union(Uint64,Int64)) =
        n < 1373653         ? (2,3) :
        n < 4759123141      ? (2,7,61) :
        n < 2152302898747   ? (2,3,5,7,11) :
        n < 3474749660383   ? (2,3,5,7,11,13) :
                              (2,325,9375,28178,450775,9780504,1795265022)

function isprime(n::Integer)
    n == 2 && return true
    (n < 2) | iseven(n) && return false
    s = trailing_zeros(n-1)
    d = (n-1) >>> s
    for a in witnesses(n)
        a < n || break
        x = powermod(a,d,n)
        x == 1 && continue
        t = s
        while x != n-1
            (t-=1) <= 0 && return false
            x = oftype(n, Base.widemul(x,x) % n)
            x == 1 && return false
        end
    end
    return true
end
