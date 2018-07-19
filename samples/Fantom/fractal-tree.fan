using fwt
using gfx

class FractalCanvas : Canvas
{
  new make () : super() {}

  Void drawTree (Graphics g, Int x1, Int y1, Int angle, Int depth)
  {
    if (depth == 0) return
    Int x2 := x1 + (angle.toFloat.toRadians.cos * depth * 10.0).toInt;
    Int y2 := y1 + (angle.toFloat.toRadians.sin * depth * 10.0).toInt;
    g.drawLine(x1, y1, x2, y2);
    drawTree(g, x2, y2, angle - 20, depth - 1);
    drawTree(g, x2, y2, angle + 20, depth - 1);
  }

  override Void onPaint (Graphics g)
  {
    drawTree (g, 400, 500, -90, 9)
  }
}

class FractalTree
{
  public static Void main ()
  {
    Window
    {
      title = "Fractal Tree"
      size = Size(800, 600)
      FractalCanvas(),
    }.open
  }
}
