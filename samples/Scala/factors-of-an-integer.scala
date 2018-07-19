  def factors(num: Int) = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }
