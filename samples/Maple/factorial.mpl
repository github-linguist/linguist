Factorial := proc(n)
    if n <= 1 then
        return 1;
    else
        return n * Factorial(n-1);
    end if;
end proc;

Factorial(6);
