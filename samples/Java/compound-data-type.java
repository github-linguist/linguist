public class Point
{
  public int x, y;
  public Point() { this(0); }
  public Point(int x0) { this(x0,0); }
  public Point(int x0, int y0) { x = x0; y = y0; }

  public static void main(String args[])
  {
    Point point = new Point(1,2);
    System.out.println("x = " + point.x );
    System.out.println("y = " + point.y );
  }
}
