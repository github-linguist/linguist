{
  // Regex Pattern Matching (Can be named or unnamed)
  a: in0.phones map $ match {
    case matches /\+(\d+)\s\((\d+)\)\s(\d+\-\d+)/ -> { country: $[0], area: $[1], number: $[2] }
    case matches /\((\d+)\)\s(\d+\-\d+)/ -> { area: $[1], number: $[2] }
    case phone matches /\((\d+)\)\s(\d+\-\d+)/ -> { area: phone[1], number: phone[2] }
  },
  // Type Pattern Matching (Can be named or unnamed)
  b: in0.object match {
    case is Object -> { object: $ }
    case is Number -> { number: $ }
    // This is how you name variables if needed
    case y is Boolean -> { boolean: y }
  },
  // Literal Pattern Matching (Can be named or unnamed)
  c: in0.value match {
    case "Emiliano" -> { string: $ }
    case 123 -> { number: $ }
    // This is how you name variables if needed
    case value: "Mariano" -> { name: value }
  },
  // Boolean Expression Pattern Matching (Always named)
  d: in0.value match {
    case x if x > 30 -> { biggerThan30: x }
    case x if x == 9 -> { nine: x }
  },
  // Default matches
  e: in0.value match {
    case "Emiliano" -> "string"
    case 3.14 -> number
    else -> "1234"
  }
}