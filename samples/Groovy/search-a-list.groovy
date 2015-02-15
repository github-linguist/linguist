def haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"]
def needles = ["Washington","Bush","Wally"]
needles.each { needle ->
    def index = haystack.indexOf(needle)
    def lastindex = haystack.lastIndexOf(needle)
    if (index < 0) {
        assert lastindex < 0
        println needle + " is not in haystack"
    } else {
        println "First index: " + index + " " + needle
        println "Last index:  " + lastindex + " " + needle
    }
}
