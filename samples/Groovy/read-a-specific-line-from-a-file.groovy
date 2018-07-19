def line = null
new File("lines.txt").eachLine { currentLine, lineNumber ->
    if (lineNumber == 7) {
        line = currentLine
    }
}
println "Line 7 = $line"
