iter fib() {
        var a = 0, b = 1;

        while true {
                yield a;
                (a, b) = (b, b + a);
        }
}
