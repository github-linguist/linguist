Catalan1 := n -> Binomial(2*n, n) - Binomial(2*n, n - 1);

Catalan2 := n -> Binomial(2*n, n)/(n + 1);

Catalan3 := function(n)
    local k, c;
    c := 1;
    k := 0;
    while k < n do
        k := k + 1;
        c := 2*(2*k - 1)*c/(k + 1);
    od;
    return c;
end;

Catalan4_memo := [1];
Catalan4 := function(n)
    if not IsBound(Catalan4_memo[n + 1]) then
        Catalan4_memo[n + 1] := Sum([0 .. n - 1], i -> Catalan4(i)*Catalan4(n - 1 - i));
    fi;
    return Catalan4_memo[n + 1];
end;


# The first fifteen: 0 to 14 !
List([0 .. 14], n -> Catalan1(n));
List([0 .. 14], n -> Catalan2(n));
List([0 .. 14], n -> Catalan3(n));
List([0 .. 14], n -> Catalan4(n));
# Same output for all four:
# [ 1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440 ]
