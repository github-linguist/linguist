Balanced := function(L)
    local c, r;
    r := 0;
    for c in L do
        if c = ']' then
            r := r - 1;
            if r < 0 then
                return false;
            fi;
        elif c = '[' then
            r := r + 1;
        fi;
    od;
    return r = 0;
end;

Balanced("");
# true

Balanced("[");
# false

Balanced("]");
# false

Balanced("[]");
# true

Balanced("][");
# false

Balanced("[[][]]");
# true

Balanced("[[[]][]]]");
# false
