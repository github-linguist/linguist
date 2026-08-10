GCD := proc(a, b)
    if b = 0 then
        return a;
    else
        return GCD(b, a mod b);
    end if;
end proc;

GCD(1071, 462);
