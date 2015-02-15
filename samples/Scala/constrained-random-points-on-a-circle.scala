object CRP extends SimpleSwingApplication {
  import scala.swing._
  import scala.swing.Swing._
  import scala.swing.{MainFrame, Panel, SimpleGUIApplication}
  import scala.swing.event._
  import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}
  import scala.util.Random
  import scala.math._

  //min/max of display-x resp. y
  val dx0,dy0 = 30
  val dxm,dym = 430

  val prefSizeX,prefSizeY = 480

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = (prefSizeX, prefSizeY)

    val xmax,ymax = 20; val xmin,ymin = -20

    case class Coord(x: Double, y: Double) {
      val dx = (((dxm-dx0)/2+x.toDouble/xmax*(dxm-dx0)/2)+dx0).toInt
      val dy = (((dym-dy0)/2-y.toDouble/ymax*(dym-dy0)/2)+dy0).toInt
    }

    case class Circle(x: Double, y: Double, r: Double, c: java.awt.Color) {
      val mdp = Coord(x,y)
      val dr = (Coord(r,0).dx-pcentre.dx)*2
      val dx = mdp.dx-dr/2
      val dy = mdp.dy-dr/2
    }

    val pcentre = Coord(0,0)
    val pxmax = Coord(xmax,0); val pxmin = Coord(xmin,0)
    val pymax = Coord(0,ymax); val pymin = Coord(0,ymin)

    //axes:
    var a_path = new geom.GeneralPath
    a_path.moveTo(pxmin.dx, pxmin.dy); a_path.lineTo(pxmax.dx, pxmax.dy) //x-axis
    a_path.moveTo(pymin.dx, pymin.dy); a_path.lineTo(pymax.dx, pymax.dy) //y-axis

    //labeling:
    val labels = List(-20,-15,-10,-5,5,10,15,20)
    labels.foreach{x=>{val p=Coord(x,0);a_path.moveTo(p.dx,p.dy-3);a_path.lineTo(p.dx,p.dy+3)}}
    labels.foreach{y=>{val p=Coord(0,y);a_path.moveTo(p.dx-3,p.dy);a_path.lineTo(p.dx+3,p.dy)}}
    val xlabels = labels.map(x=>{val p=Coord(x,0); Triple(x.toString,p.dx-3,p.dy+20)})
    val ylabels = labels.map(y=>{val p=Coord(0,y); Triple(y.toString,p.dx-20,p.dy+5)})

    //circles:
    val circles = cs.map{case (x,y,r,c)=>Circle(x,y,r,cm(c))}

    //points:
    val points = new Iterator[Int] {val r = new Random; def next = r.nextInt(31)-15; def hasNext = true}.toStream
      .zip(new Iterator[Int] {val r = new Random; def next = r.nextInt(31)-15; def hasNext = true}.toStream)
      .map{case (x,y)=>(x,y,hypot(x,y))}.filter{case (x,y,r)=>r>=10&&r<=15}.take(100).toList
      .map{case (x,y,r) => new Rectangle(Coord(x,y).dx-2, Coord(x,y).dy-2, 4, 4)}

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      circles.foreach{c=>{g.setColor(c.c); g.drawOval(c.dx,c.dy,c.dr,c.dr)}}
      g.setColor(cm("r")); points.foreach(g.draw(_))
      g.setColor(cm("s")); g.draw(a_path)
      xlabels.foreach{case (text,px,py)=>g.drawString(text,px,py)}
      ylabels.foreach{case (text,px,py)=>g.drawString(text,px,py)}
    }
  }

  val cm = Map("b"->Color.blue,"g"->Color.green,"r"->Color.red,"s"->Color.black)
  val cs = List((0,0,10,"b"),(0,0,15,"g")) //circle position and colour

  def top = new MainFrame {
    title = "Rosetta Code >>> Task: Constrained random points on a circle | Language: Scala"
    contents = ui
  }
}
