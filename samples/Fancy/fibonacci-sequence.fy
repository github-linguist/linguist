class Fixnum {
  def fib {
    match self -> {
      case 0 -> 0
      case 1 -> 1
      case _ -> self - 1 fib + (self - 2 fib)
    }
  }
}

15 times: |x| {
  x fib println
}
