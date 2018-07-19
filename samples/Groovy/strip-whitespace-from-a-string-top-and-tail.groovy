//Grape setup to get library
@Grab('org.apache.commons:commons-lang3:3.0.1')
import static org.apache.commons.lang3.StringUtils.*

def abc = '\r\n\t  abc  \r\n\t'

def printTest = {
    println ('|' + it + '|')
}

println 'Unstripped\n------------'
printTest abc

println '============\n\nStripped\n------------'
printTest strip(abc)

println '============\n\nLeft Stripped\n------------'
printTest stripStart(abc, null)

println '============\n\nRight Stripped\n------------'
printTest stripEnd(abc, null)
println '============'
