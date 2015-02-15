class Main
{
  public static Void main ()
  {
    q := Queue()
    q.push (1)
    q.push ("a")
    echo ("Is empty? " + q.isEmpty)
    echo ("Element: " + q.pop)
    echo ("Element: " + q.pop)
    echo ("Is empty? " + q.isEmpty)
    try { q.pop } catch (Err e) { echo (e.msg) }
  }
}
