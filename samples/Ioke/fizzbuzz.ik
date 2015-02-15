(1..100) each(x,
  cond(
    (x % 15) zero?, "FizzBuzz" println,
    (x % 3) zero?, "Fizz" println,
    (x % 5) zero?, "Buzz" println
  )
)
