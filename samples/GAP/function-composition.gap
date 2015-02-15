Composition := function(f, g)
    return x -> f(g(x));
end;

h := Composition(x -> x+1, x -> x*x);
h(5);
# 26
