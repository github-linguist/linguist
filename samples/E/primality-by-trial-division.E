def isPrime(n :int) {
    if (n == 2) {
        return true
    } else if (n <= 1 || n %% 2 == 0) {
        return false
    } else {
        def limit := (n :float64).sqrt().ceil()
        var divisor := 1
        while ((divisor += 2) <= limit) {
            if (n %% divisor == 0) {
                return false
            }
        }
        return true
    }
}
