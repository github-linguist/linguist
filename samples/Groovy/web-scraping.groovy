def time = "unknown"
def text = new URL('http://tycho.usno.navy.mil/cgi-bin/timer.pl').eachLine { line ->
    def matcher = (line =~ "<BR>(.+) UTC")
    if (matcher.find()) {
        time = matcher[0][1]
    }
}
println "UTC Time was '$time'"
