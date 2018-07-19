def lookAndSayNext(number :int) {
  var seen := null
  var count := 0
  var result := ""
  def put() {
    if (seen != null) {
      result += count.toString(10) + E.toString(seen)
    }
  }
  for ch in number.toString(10) {
    if (ch != seen) {
      put()
      seen := ch
      count := 0
    }
    count += 1
  }
  put()
  return __makeInt(result, 10)
}

var number := 1
for _ in 1..20 {
  println(number)
  number := lookAndSayNext(number)
}
