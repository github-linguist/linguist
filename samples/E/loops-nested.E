def array := accum [] for i in 1..5 { _.with(accum [] for i in 1..5 { _.with(entropy.nextInt(20) + 1) }) }

escape done {
    for row in array {
        for x in row {
            print(`$x$\t`)
            if (x == 20) {
                done()
            }
        }
        println()
    }
}
println("done.")
