class SEDOL(s: String) {
  require(s.size == 6 || s.size == 7, "SEDOL length must be 6 or 7 characters")
  require(s.size == 6 || s(6).asDigit == chksum, "Incorrect SEDOL checksum")
  require(s forall (c => !("aeiou" contains c.toLower)), "Vowels not allowed in SEDOL")
  def chksum = 10 - ((s zip List(1, 3, 1, 7, 3, 9) map { case (c, w) => c.asDigit * w } sum) % 10)
  override def toString = s.take(6) + chksum
}
