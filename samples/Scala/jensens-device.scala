class MyInt { var i: Int = _ }
val i = new MyInt
def sum(i: MyInt, lo: Int, hi: Int, term: => Double) = {
  var temp = 0.0
  i.i = lo
  while(i.i <= hi) {
    temp = temp + term
    i.i += 1
  }
  temp
}
sum(i, 1, 100, 1.0 / i.i)
