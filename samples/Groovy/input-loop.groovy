def lineMap = [:]
System.in.eachLine { line, i ->
    lineMap[i] = line
}
lineMap.each { println it }
