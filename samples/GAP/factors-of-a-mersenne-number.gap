MersenneSmallFactor := function(n)
    local k, m, d;
    if IsPrime(n) then
        d := 2*n;
        m := 1;
        for k in [1 .. 1000000] do
            m := m + d;
            if PowerModInt(2, n, m) = 1 then
                return m;
            fi;
        od;
    fi;
    return fail;
end;


# If n is not prime, fail immediately
MersenneSmallFactor(15);
# fail

MersenneSmallFactor(929);
# 13007

MersenneSmallFactor(1009);
# 3454817

# We stop at k = 1000000 in 2*k*n + 1, so it may fail if 2^n - 1 has only larger factors
MersenneSmallFactor(101);
# fail

FactorsInt(2^101-1);
# [ 7432339208719, 341117531003194129 ]
