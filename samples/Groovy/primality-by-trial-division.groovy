def isPrime = {
    it == 2 ||
    it > 1 &&
    (2..Math.max(2, (int) Math.sqrt(it))).every{ k -> it % k != 0 }
}

(0..20).grep(isPrime)
