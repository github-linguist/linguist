case class HowMany(asker: Actor)

val printer = actor {
  var count = 0
  while (true) {
    receive {
      case line: String =>
        print(line); count = count + 1
      case HowMany(asker: Actor) => asker ! count; exit()
    }
  }
}

def reader(printer: Actor) {
  scala.io.Source.fromFile("c:\\input.txt").getLines foreach { printer ! _ }
  printer ! HowMany(
    actor {
      receive {
        case count: Int => println("line count = " + count)
      }
    })
}

reader(printer)
