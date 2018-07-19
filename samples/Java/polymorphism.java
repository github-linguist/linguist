class Point {
   protected int x, y;
   public Point() { this(0); }
   public Point(int x0) { this(x0, 0); }
   public Point(int x0, int y0) { x = x0; y = y0; }
   public Point(Point p) { this(p.x, p.y); }
   public int getX() { return x; }
   public int getY() { return y; }
   public int setX(int x0) { x = x0; }
   public int setY(int y0) { y = y0; }
   public void print() { System.out.println("Point"); }
}

public class Circle extends Point {
   private int r;
   public Circle(Point p) { this(p, 0); }
   public Circle(Point p, int r0) { super(p); r = r0; }
   public Circle() { this(0); }
   public Circle(int x0) { this(x0, 0); }
   public Circle(int x0, int y0) { this(x0, y0, 0); }
   public Circle(int x0, int y0, int r0) { super(x0, y0); r = r0; }
   public Circle(Circle c) { this(c.x, c.y, c.r); }
   public int getR() { return r; }
   public int setR(int r0) { r = r0; }
   public void print() { System.out.println("Circle"); }

   public static void main(String args[]) {
      Point p = new Point();
      Point c = new Circle();
      p.print();
      c.print();
   }
}
