# The Loop function will apply some function to every tuple built by taking
# the i-th element of each list. If one of them is exhausted before the others,
# the loop continues at its begining. Only the longests lists will be precessed only once.
Loop := function(a, f)
    local i, j, m, n, v;
    n := Length(a);
    v := List(a, Length);
    m := Maximum(v);
    for j in [1 .. m] do
        f(List([1 .. n], i -> a[i][1 + RemInt(j - 1, v[i])]));
    od;
end;

# Here we simply print each "row"
f := function(u)
    Perform(u, Print);
    Print("\n");
end;

Loop([["a", "b", "c"], ["A", "B", "C"], [1, 2, 3]], f);

aA1
bB2
cC3

Loop([["a", "b"], ["A", "B", "C", "D", "E"], [1, 2, 3]], f);

aA1
bB2
aC3
bD1
aE2
