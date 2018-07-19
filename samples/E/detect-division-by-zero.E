def divide(numerator, denominator) {
    def floatQuotient := numerator / denominator
    if (floatQuotient.isNaN() || floatQuotient.isInfinite()) {
        return ["zero denominator"]
    } else {
        return ["ok", floatQuotient]
    }
}
