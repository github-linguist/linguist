def bottles(n) {
  return switch (n) {
    match ==0 { "No bottles" }
    match ==1 { "1 bottle" }
    match _   { `$n bottles` }
  }
}
for n in (1..99).descending() {
  println(`${bottles(n)} of beer on the wall,
${bottles(n)} of beer.
Take one down, pass it around,
${bottles(n.previous())} of beer on the wall.
`)
}
