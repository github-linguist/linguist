def fib(n) {
    var s := [0, 1]
    for _ in 0..!n {
        def [a, b] := s
        s := [b, a+b]
    }
    return s[0]
}
