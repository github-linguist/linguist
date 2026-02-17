Fib := proc(n, memo := table())
    if n <= 1 then return n; end if;
    if not assigned(memo[n]) then
       memo[n] := Fib(n-1, memo) + Fib(n-2, memo);
    end if;
    return memo[n];
end proc;

Fib(12);
