# You can't break an outer loop unless you return from the whole function.
n := 40;
a := List([1 .. n], i -> List([1 .. n], j -> Random(1, 20)));;

Find := function(a, x)
    local i, j, n;
    n := Length(a);
    for i in [1 .. n] do
        for j in [1 .. n] do
            if a[i][j] = x then
                return [i, j];
            fi;
        od;
    od;
    return fail;
end;

Find(a, 20);
