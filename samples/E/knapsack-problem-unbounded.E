pragma.enable("accumulator")

/** A data type representing a bunch of stuff (or empty space). */
def makeQuantity(value, weight, volume, counts) {
  def quantity {
    to __printOn(out) {
      for name => n in counts { out.print(`$n $name  `) }
      out.print(`(val=$value wt=$weight vol=$volume)`)
    }
    to value () { return value  }
    to weight() { return weight }
    to volume() { return volume }
    to counts() { return counts }
    to subtract(other) { return quantity + other * -1 }
    to add(other) {
      return makeQuantity(value  + other.value (),
                          weight + other.weight(),
                          volume + other.volume(),
                          accum counts for name => n in other.counts() { _.with(name, n+counts.fetch(name, fn {0})) })
    }
    to multiply(scalar) {
      return makeQuantity(value  * scalar,
                          weight * scalar,
                          volume * scalar,
                          accum [].asMap() for name => n in counts { _.with(name, n*scalar) })
    }
    /** a.fit(b) the greatest integer k such that a - b * k does not have negative weight or volume. */
    to fit(item) {
      return (weight // item.weight()) \
         .min(volume // item.volume())
    }
  }
  return quantity
}

/** Fill the space with the treasures, returning candidate results as spaceAvailable - the items. */
def fill(spaceAvailable, treasures) {
  if (treasures.size().isZero()) { # nothing to pick
    return [spaceAvailable]
  }

  # Pick one treasure type
  def [unit] + otherTreasures := treasures

  var results := []
  for count in (0..spaceAvailable.fit(unit)).descending() {
    results += fill(spaceAvailable - unit * count, otherTreasures)
    if (otherTreasures.size().isZero()) {
      break # If there are no further kinds, there is no point in taking less than the most
    }
  }
  return results
}

def chooseBest(emptyKnapsack, treasures) {
  var maxValue := 0
  var best := []
  for result in fill(emptyKnapsack, treasures) {
    def taken := emptyKnapsack - result # invert the backwards result fill() returns
    if (taken.value() > maxValue) {
      best := [taken]
      maxValue := taken.value()
    } else if (taken.value() <=> maxValue) {
      best with= taken
    }
  }
  return best
}

def printBest(emptyKnapsack, treasures) {
  for taken in chooseBest(emptyKnapsack, treasures) { println(`  $taken`) }
}

def panacea := makeQuantity(3000, 0.3, 0.025, ["panacea" => 1])
def ichor   := makeQuantity(1800, 0.2, 0.015, ["ichor"   => 1])
def gold    := makeQuantity(2500, 2.0, 0.002, ["gold"    => 1])
def emptyKnapsack \
            := makeQuantity(   0,  25, 0.250, [].asMap())

printBest(emptyKnapsack, [panacea, ichor, gold])
