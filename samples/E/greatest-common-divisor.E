def gcd(var u :int, var v :int) {
    while (v != 0) {
        def r := u %% v
        u := v
        v := r
    }
    return u.abs()
}
