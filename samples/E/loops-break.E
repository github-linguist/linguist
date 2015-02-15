while (true) {
    def a := entropy.nextInt(20)
    print(a)
    if (a == 10) {
        println()
        break
    }
    println(" ", entropy.nextInt(20))
}
