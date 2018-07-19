import akka.actor.{ Actor, ActorSystem, Props }
import scala.collection.immutable.TreeSet
import scala.xml.XML

// Reports a list with all languages recorded in the Wiki

private object Acquisition {
  val (endPoint, prefix) = ("http://rosettacode.org/mw/api.php", "Category:")
  val (maxPlaces, correction) = (50, 2)

  def convertPathArgsToURL(endPoint: String, pathArgs: Map[String, String]) = {
    pathArgs.map(argPair => argPair._1 + "=" + argPair._2)
      .mkString(endPoint + (if (pathArgs.nonEmpty) "?" else ""), "&", "")
  }

  /* The categories include a page for the language and a count of the pages
   * linked therein, this count is the data we need to scrape.
   * Reports a list with language, count pair recorded in the Wiki
   * All strings starts with the prefixes "Category:"
   */
  def mineCatos = {
    val endPoint = "http://rosettacode.org/mw/index.php"
    Concurrent.logInfo("Acquisition of categories started.")
    val categories =
      (XML.load(convertPathArgsToURL(endPoint,
        Map("title" -> "Special:Categories", "limit" -> "5000"))) \\ "ul" \ "li")
        .withFilter(p => (p \ "a" \ "@title").text.startsWith(prefix))
        .map // Create a tuple pair, eg. ("Category:Erlang", 195)
        { cat =>
          ((cat \ "a" \ "@title").text, // Takes the sibling of "a" and extracts the number
            ("[0-9]+".r.findFirstIn((cat.child.drop(1)).text).getOrElse("0").toInt))
        }
    Concurrent.logInfo(s"Got ${categories.size} categories..")
    categories
  }

  // The languages
  // All strings starts with the prefixes "Category:"
  def mineLangs = {
    Concurrent.logInfo("Acquisition of languages started...")
    val languages = (
      TreeSet[String]() ++ (XML.load(convertPathArgsToURL(endPoint, Map("action" -> "query",
        "list" -> "categorymembers",
        "cmtitle" -> (prefix + "Programming_Languages"),
        "cmlimit" -> "505", "format" -> "xml"))) \\
        "categorymembers" \ "cm").map(c => (c \ "@title").text))
    Concurrent.logInfo(s"Got ${languages.size} languages..")
    languages
  }

  def joinRosettaCodeWithLanguage(catos: Seq[(String, Int)],
                                  langs: TreeSet[String]) =
    for {
      cato <- catos //Clean up the tuple pairs, eg ("Category:Erlang", 195) becomes ("Erlang", 192)
      if langs.exists(_ == cato._1)
    } yield (cato._1.drop(prefix.size), cato._2 - correction max 0) // Correct count

  def printScrape(languages: TreeSet[String], category: Seq[(String, Int)]) {

    val join = joinRosettaCodeWithLanguage(category, languages)
    val total = join.foldLeft(0)(_ + _._2)

    Concurrent.logInfo("Data processed")

    println(f"\nTop$maxPlaces%3d Rosetta Code Languages by Popularity as ${new java.util.Date}%tF:\n")
    (join.groupBy(_._2).toSeq.sortBy(-_._1).take(maxPlaces) :+ (0, Seq(("...", 0))))
      .zipWithIndex // Group the ex aequo
      .foreach {
        case ((score, languages), rank) =>
          println(f"${rank + 1}%2d. $score%3d - ${languages.map(_._1).mkString(", ")}")
      }

    println(s"\nCross section yields ${join.size} languages, total of $total solutions")
    println(s"Resulting average is ${total / join.size} solutions per language")
  }

  def printScrape(): Unit = printScrape(mineLangs, mineCatos)
} // object Acquisition

private object Concurrent extends AppCommons {
  var (category: Option[Seq[(String, Int)]], language: Option[TreeSet[String]]) = (None, None)

  class Worker extends Actor {
    def receive = {
      case 'Catalogue => sender ! Acquisition.mineCatos
      case 'Language  => sender ! Acquisition.mineLangs
    }
  }

  class Listener extends Actor { // Create and signal the worker actors
    context.actorOf(Props[Worker], "worker0") ! 'Catalogue
    context.actorOf(Props[Worker], "worker1") ! 'Language

    def printCompleteScape() =
      if (category.isDefined && language.isDefined) {
        Acquisition.printScrape(language.get, category.get)
        context.system.shutdown
        appEnd()
      }

    def receive = {
      case content: TreeSet[String] => {
        language = Some(content)
        printCompleteScape()
      }
      case content: Seq[(String, Int)] => {
        category = Some(content)
        printCompleteScape()
      }
      case whatever => logInfo(whatever.toString)
    } // def receive
  } // class Listener
} // object Concurrent

trait AppCommons {
  val execStart: Long = System.currentTimeMillis()
  System.setProperty("http.agent", "*")

  def logInfo(info: String) {
    println(f"[Info][${System.currentTimeMillis() - execStart}%5d ms]" + info)
  }

  def appEnd() { logInfo("Run succesfully completed") }
}

// Main entry for sequential version (slower)
object GhettoParserSeq extends App with AppCommons {
  Concurrent.logInfo("Sequential version started")
  Acquisition.printScrape()
  appEnd()
}

// Entry for parallel version (faster)
object GhettoParserPar extends App {
  Concurrent.logInfo("Parallel version started")
  ActorSystem("Main").actorOf(Props[Concurrent.Listener])
}
