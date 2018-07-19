pragma.enable("accumulator")
accum 0 for x in 1..1000 { _ + 1 / x ** 2 }
