type Eatable = { def eat: Unit }

class FoodBox(coll: List[Eatable])

case class Fish(name: String) {
  def eat {
    println("Eating "+name)
  }
}

val foodBox = new FoodBox(List(new Fish("salmon")))
