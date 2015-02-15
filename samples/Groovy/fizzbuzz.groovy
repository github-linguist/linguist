for (i in 1..100) {
  println "${i%3?'':'Fizz'}${i%5?'':'Buzz'}" ?: i
}
