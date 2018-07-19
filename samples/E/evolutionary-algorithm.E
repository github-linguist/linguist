pragma.syntax("0.9")
pragma.enable("accumulator")

def target := "METHINKS IT IS LIKE A WEASEL"
def alphabet := "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
def C := 100
def RATE := 0.05

def randomCharString() {
  return E.toString(alphabet[entropy.nextInt(alphabet.size())])
}

def fitness(string) {
    return accum 0 for i => ch in string {
      _ + (ch == target[i]).pick(1, 0)
    }
}

def mutate(string, rate) {
  return accum "" for i => ch in string {
    _ + (entropy.nextDouble() < rate).pick(randomCharString(), E.toString(ch))
  }
}

def weasel() {
  var parent := accum "" for _ in 1..(target.size()) { _ + randomCharString() }
  var generation := 0

  while (parent != target) {
    println(`$generation $parent`)
    def copies := accum [] for _ in 1..C { _.with(mutate(parent, RATE)) }
    var best := parent
    for c in copies {
      if (fitness(c) > fitness(best)) {
        best := c
      }
    }
    parent := best
    generation += 1
  }
  println(`$generation $parent`)
}

weasel()
