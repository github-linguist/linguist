System.print("Hello, world!")

class Wren {
  flyTo(city) {
    System.print("Flying to %(city)")
  }
}

// I am a comment

var adjectives = Fiber.new {
  ["small", "clean", "fast"].each {|word| Fiber.yield(word) }
}

while (!adjectives.isDone) System.print(adjectives.call())
