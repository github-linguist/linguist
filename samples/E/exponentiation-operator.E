def power(base, exponent :int) {
    var r := base
    if (exponent < 0) {
        for _ in exponent..0 { r /= base }
    } else if (exponent <=> 0) {
        return 1
    } else {
        for _ in 2..exponent { r *= base }
    }
    return r
}
