compose = ( f, g ) -> ( x ) -> f g x

# Example
add2 = ( x ) -> x + 2
mul2 = ( x ) -> x * 2

mulFirst = compose add2, mul2
addFirst = compose mul2, add2

console.log "add2 2 #=> #{ add2 2 }"
console.log "mul2 2 #=> #{ mul2 2 }"
console.log "mulFirst 2 #=> #{ mulFirst 2 }"
console.log "addFirst 2 #=> #{ addFirst 2 }"
