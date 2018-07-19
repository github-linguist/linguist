def printMultTable = { size = 12 ->
    assert size > 1

    // factor1 line
    print '  |'; (1..size).each { f1 -> printf('%4d', f1) }; println ''

    // dividing line
    print '--+'; (1..size).each { printf('----', it) }; println ''

    // factor2 result lines
    (1..size).each { f2 ->
        printf('%2d|', f2)
        (1..<f2).each{ print '    ' }
        (f2..size).each{ f1 -> printf('%4d', f1*f2) }
        println ''
    }
}

printMultTable()
