import java.net._
val encoded="http%3A%2F%2Ffoo%20bar%2F"
val decoded=URLDecoder.decode(encoded, "UTF-8")
println(decoded)   // -> http://foo bar/
