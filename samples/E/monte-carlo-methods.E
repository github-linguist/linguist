def pi(n) {
    var inside := 0
    for _ ? (entropy.nextFloat() ** 2 + entropy.nextFloat() ** 2 < 1) in 1..n {
         inside += 1
    }
    return inside * 4 / n
}
