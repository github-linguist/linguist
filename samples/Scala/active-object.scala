object ActiveObject {

  class Integrator {

    import java.util._
    import scala.actors.Actor._

    case class Pulse(t: Double)
    case class Input(k: Double => Double)
    case object Output
    case object Bye

    val timer = new Timer(true)
    var k: Double => Double = (_ => 0.0)
    var s: Double = 0.0
    var t0: Double = 0.0

    val handler = actor {
      loop {
        react {
          case Pulse(t1) => s += (k(t1) + k(t0)) * (t1 - t0) / 2.0; t0 = t1
          case Input(k) => this.k = k
          case Output => reply(s)
          case Bye => timer.cancel; exit
        }
      }
    }

    timer.scheduleAtFixedRate(new TimerTask {
      val start = System.currentTimeMillis
      def run { handler ! Pulse((System.currentTimeMillis - start) / 1000.0) }
    }, 0, 10) // send Pulse every 10 ms

    def input(k: Double => Double) = handler ! Input(k)
    def output = handler !? Output
    def bye = handler ! Bye
  }

  def main(args: Array[String]) {
    val integrator = new Integrator
    integrator.input(t => Math.sin(2.0 * Math.Pi * 0.5 * t))
    Thread.sleep(2000)
    integrator.input(_ => 0.0)
    Thread.sleep(500)
    println(integrator.output)
    integrator.bye
  }
}
