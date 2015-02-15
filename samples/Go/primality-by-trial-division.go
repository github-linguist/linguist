func isPrime(p int) bool {
    if p < 2 { return false }
    if p == 2 { return true }
    if p % 2 == 0 { return false }

    for i := 3; i*i < p; i += 2 {
        if p % i == 0 { return false }
    }
    return true
}
