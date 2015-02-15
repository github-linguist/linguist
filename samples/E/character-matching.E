def f(string1, string2) {
    println(string1.startsWith(string2))

    var index := 0
    while ((index := string1.startOf(string2, index)) != -1) {
        println(`at $index`)
        index += 1
    }

    println(string1.endsWith(string2))
}
