def Y[A, B](f: (A ⇒ B) ⇒ (A ⇒ B)): A ⇒ B = f(Y(f))(_)

def fib(n: Int): Option[Int] =
  if (n < 0) None
  else Some(Y[Int, Int](f ⇒ i ⇒
    if (i < 2) 1
    else f(i - 1) + f(i - 2))(n))

-2 to 5 map (n ⇒ (n, fib(n))) foreach println
