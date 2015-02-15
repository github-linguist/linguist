import java.net.{URL, URLEncoder}
import scala.io.Source.fromURL

val allTasksURL = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
val allTasks = xml.parsing.XhtmlParser(fromURL(new URL(allTasksURL)))

val regexExpr = "(?i)==\\{\\{header\\|".r
def oneTaskURL(title: String) = "http://www.rosettacode.org/w/index.php?title=%s&action=raw" format URLEncoder.encode(title.replace(' ', '_'), "UTF-8")
def count(title: String) =  regexExpr findAllIn fromURL(new URL(oneTaskURL(title)))(io.Codec.UTF8).mkString length

val counts = for (task <- allTasks \\ "cm" \\ "@title" map (_.text)) yield scala.actors.Futures.future((task, count(task)))

counts map (_.apply) map Function.tupled("%s: %d examples." format (_, _)) foreach println
println("\nTotal: %d examples." format (counts map (_.apply._2) sum))
