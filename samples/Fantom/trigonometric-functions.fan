class Main
{
  public static Void main ()
  {
    Float r := Float.pi / 4
    echo (r.sin)
    echo (r.cos)
    echo (r.tan)
    echo (r.asin)
    echo (r.acos)
    echo (r.atan)
    // and from degrees
    echo (45.0f.toRadians.sin)
    echo (45.0f.toRadians.cos)
    echo (45.0f.toRadians.tan)
    echo (45.0f.toRadians.asin)
    echo (45.0f.toRadians.acos)
    echo (45.0f.toRadians.atan)
  }
}
