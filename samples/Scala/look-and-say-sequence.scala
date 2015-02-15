def lookAndSay(seed: BigInt) = {
  val s = seed.toString
  ( 1 until s.size).foldLeft((1, s(0), new StringBuilder)) {
    case ((len, c, sb), index) if c != s(index) => sb.append(len); sb.append(c); (1, s(index), sb)
    case ((len, c, sb), _) => (len + 1, c, sb)
  } match {
    case (len, c, sb) => sb.append(len); sb.append(c); BigInt(sb.toString)
  }
}

def lookAndSayIterator(seed: BigInt) = Iterator.iterate(seed)(lookAndSay)
