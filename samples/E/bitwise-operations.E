def bitwise(a :int, b :int) {
   println(`Bitwise operations:
   a AND b: ${a & b}
   a OR b: ${a | b}
   a XOR b: ${a ^ b}
   NOT a: " + ${~a}
   a left shift b: ${a << b}
   a right shift b: ${a >> b}
`)
}
