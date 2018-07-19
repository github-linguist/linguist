Y := function(f)
    local u;
    u := x -> x(x);
    return u(y -> f(a -> y(y)(a)));
end;

fib := function(f)
    local u;
    u := function(n)
        if n < 2 then
            return n;
        else
            return f(n-1) + f(n-2);
        fi;
    end;
    return u;
end;

Y(fib)(10);
# 55

fac := function(f)
    local u;
    u := function(n)
        if n < 2 then
            return 1;
        else
            return n*f(n-1);
        fi;
    end;
    return u;
end;

Y(fac)(8);
# 40320
