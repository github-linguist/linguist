enum Fruit { apple, banana, cherry }

enum ValuedFruit {
    apple(1), banana(2), cherry(3);
    def value
    ValuedFruit(val) {value = val}
    String toString() { super.toString() + "(${value})" }
}

println Fruit.values()
println ValuedFruit.values()
