def priceFraction(value) {
    assert value >= 0.0 && value <= 1.0

    def priceMappings = [(0.06): 0.10, (0.11): 0.18, (0.16): 0.26, (0.21): 0.32, (0.26): 0.38,
         (0.31): 0.44, (0.36): 0.50, (0.41): 0.54, (0.46): 0.58, (0.51): 0.62,
         (0.56): 0.66, (0.61): 0.70, (0.66): 0.74, (0.71): 0.78, (0.76): 0.82,
         (0.81): 0.86, (0.86): 0.90, (0.91): 0.94, (0.96): 0.98]

    for (price in priceMappings.keySet()) {
        if (value < price) return priceMappings[price]
    }
    1.00
}

for (def v = 0.00; v <= 1.00; v += 0.01) {
    println "$v --> ${priceFraction(v)}"
}
