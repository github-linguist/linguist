function factors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, f, [f*p^j for j in 1:e])
    end
    return length(f) == 1 ? [one(n), n] : sort!(f)
end

function multorder(a, m)
    gcd(a,m) == 1 || error("$a and $m are not coprime")
    res = one(m)
    for (p,e) in factor(m)
        m = p^e
        t = div(m, p) * (p-1)
        for f in factors(t)
            if powermod(a, f, m) == 1
                res = lcm(res, f)
                break
            end
        end
    end
    res
end
