matchAt = (s, frag, i) ->
  s[i...i+frag.length] == frag

startsWith = (s, frag) ->
  matchAt s, frag, 0

endsWith = (s, frag) ->
  matchAt s, frag, s.length - frag.length

matchLocations = (s, frag) ->
  (i for i in [0..s.length - frag.length] when matchAt s, frag, i)

console.log startsWith "tacoloco", "taco" # true
console.log startsWith "taco", "tacoloco" # false
console.log startsWith "tacoloco", "talk" # false
console.log endsWith "tacoloco", "loco" # true
console.log endsWith "loco", "tacoloco" # false
console.log endsWith "tacoloco", "yoco" # false
console.log matchLocations "bababab", "bab" # [0,2,4]
console.log matchLocations "xxx", "x" # [0,1,2]
