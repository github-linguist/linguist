interp.waitAtTop(when (def html := <http://tycho.usno.navy.mil/cgi-bin/timer.pl>.getText()) -> {
    def rx`(?s).*>(@time.*? UTC).*` := html
    println(time)
})
