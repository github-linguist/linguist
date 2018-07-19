LucasLehmer := function(n)
    local i, m, s;
    if n = 2 then
        return true;
    elif not IsPrime(n) then
        return false;
    else
        m := 2^n - 1;
        s := 4;
        for i in [3 .. n] do
            s := RemInt(s*s, m) - 2;
        od;
        return s = 0;
    fi;
end;

Filtered([1 .. 2000], LucasLehmer);
[2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279]
