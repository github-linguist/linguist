# Quick and dirty solution, checking all permutations without backtracking (thus it's slow)
IsSafe := function(a)
    local n, i, j;
    n := Length(a);
    for i in [1 .. n - 1] do
        for j in [i + 1 .. n] do
            if AbsInt(a[j] - a[i]) = j - i then
                return false;
            fi;
        od;
    od;
    return true;
end;

Queens := function(n)
    local p, a, v;
    v := [];
    for p in SymmetricGroup(n) do
        a := List([1 .. n], i -> i^p);
        if IsSafe(a) then
            Add(v, a);
        fi;
    od;
    return v;
end;

v := Queens(8);;
Length(v);
PrintArray(PermutationMat(PermListList([1 .. 8], v[1]), 8));
[ [  0,  0,  1,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  0,  0,  1,  0,  0 ],
  [  0,  1,  0,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  0,  1,  0,  0,  0 ],
  [  0,  0,  0,  0,  0,  0,  0,  1 ],
  [  1,  0,  0,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  0,  0,  0,  1,  0 ],
  [  0,  0,  0,  1,  0,  0,  0,  0 ] ]
