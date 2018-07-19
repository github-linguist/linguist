def factors(x :(int > 0)) {
    var xfactors := []
    for f ? (x % f <=> 0) in 1..x {
      xfactors with= f
    }
    return xfactors
}
