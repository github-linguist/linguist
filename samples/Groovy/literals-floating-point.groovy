println 1.00f    // float (IEEE-32)
println 1.00d    // double (IEEE-64)
println 1.00     // BigDecimal (scaled BigInteger)
println 1.00g    // BigDecimal
println 1.00e0   // BigDecimal

assert 1.00f  instanceof Float
assert 1.00d  instanceof Double
assert 1.00   instanceof BigDecimal
assert 1.00g  instanceof BigDecimal
assert 1.00e0 instanceof BigDecimal
