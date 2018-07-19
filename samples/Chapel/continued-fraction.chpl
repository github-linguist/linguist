proc calc(f, n) {
        var r = 0.0;

        for k in 1..n by -1 {
                var v = f.pair(k);
                r = v(2) / (v(1) + r);
        }

        return f.pair(0)(1) + r;
}

record Sqrt2 {
        proc pair(n) {
                return (if n == 0 then 1 else 2,
                        1);
        }
}

record Napier {
        proc pair(n) {
                return (if n == 0 then 2 else n,
                        if n == 1 then 1 else n - 1);
        }
}
record Pi {
        proc pair(n) {
                return (if n == 0 then 3 else 6,
                        (2*n - 1)**2);
        }
}

config const n = 200;
writeln(calc(new Sqrt2(), n));
writeln(calc(new Napier(), n));
writeln(calc(new Pi(), n));
