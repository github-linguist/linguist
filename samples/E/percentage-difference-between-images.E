def imageDifference(a, b) {
  require(a.width() == b.width())
  require(a.height() == b.height())
  def X := 0..!(a.width())
  def Y := 0..!(a.height())

  var sumByteDiff := 0
  for y in Y {
    for x in X {
      def ca := a[x, y]
      def cb := b[x, y]
      sumByteDiff += (ca.rb() - cb.rb()).abs() \
                   + (ca.gb() - cb.gb()).abs() \
                   + (ca.bb() - cb.bb()).abs()
    }
    println(y)
  }
  return sumByteDiff / (255 * 3 * a.width() * a.height())
}

def imageDifferenceTask() {
  println("Read 1...")
  def a := readPPM(<import:java.io.makeFileInputStream>(<file:Lenna50.ppm>))
  println("Read 2...")
  def b := readPPM(<import:java.io.makeFileInputStream>(<file:Lenna100.ppm>))
  println("Compare...")
  def d := imageDifference(a, b)
  println(`${d * 100}% different.`)
}
