class APlusB
{
  public static Void main ()
  {
    echo ("Enter two numbers: ")
    Str input := Env.cur.in.readLine
    Int sum := 0
    input.split.each |n| { sum += n.toInt }
    echo (sum)
  }
}
