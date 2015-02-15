countSubstring = (str, substr) ->
  n = 0
  i = 0
  while (pos = str.indexOf(substr, i)) != -1
    n += 1
    i = pos + substr.length
  n

console.log countSubstring "the three truths", "th"
console.log countSubstring "ababababab", "abab"
