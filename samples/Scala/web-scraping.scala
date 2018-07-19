import scala.io.Source

object WebTime extends Application {
	val text = Source.fromURL("http://tycho.usno.navy.mil/cgi-bin/timer.pl")
	val utc = text.getLines.find(_.contains("UTC"))
	utc match {
		case Some(s) => println(s.substring(4))
		case _ => println("error")
	}
}
