def sum = 0
def sumSq = 0
def count = 0
[2,4,4,4,5,5,7,9].each {
    sum += it
    sumSq += it*it
    count++
    println "running std.dev.: ${(sumSq/count - (sum/count)**2)**0.5}"
}
