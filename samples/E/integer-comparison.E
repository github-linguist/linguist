def compare(a :int, b :int) {
  println(if (a < b)        { `$a < $b` } \
          else if (a <=> b) { `$a = $b` } \
          else if (a > b)   { `$a > $b` } \
          else              { `You're calling that an integer?` })
}
