def nextCarpet(carpet: List[String]): List[String] = (
  carpet.map(x => x + x + x) :::
  carpet.map(x => x + x.replace('#', ' ') + x) :::
  carpet.map(x => x + x + x))

def sierpinskiCarpets(n: Int) = (Iterator.iterate(List("#"))(nextCarpet) drop n next) foreach println
