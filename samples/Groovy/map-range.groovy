def mapRange(a1, a2, b1, b2, s) {
    b1 + ((s - a1) * (b2 - b1)) / (a2 - a1)
}

(0..10).each { s ->
    println(s + " in [0, 10] maps to " + mapRange(0, 10, -1, 0, s) + " in [-1, 0].")
}
