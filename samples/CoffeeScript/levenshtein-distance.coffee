levenshtein = (str1, str2) ->
  # more of less ported simple algorithm from JS
  m = str1.length
  n = str2.length
  d = []

  return n  unless m
  return m  unless n

  d[i] = [i] for i in [0..m]
  d[0][j] = j for j in [1..n]

  for i in [1..m]
    for j in [1..n]
      if str1[i-1] is str2[j-1]
        d[i][j] = d[i-1][j-1]
      else
        d[i][j] = Math.min(
          d[i-1][j]
          d[i][j-1]
          d[i-1][j-1]
        ) + 1

  d[m][n]

console.log levenshtein("kitten", "sitting")
console.log levenshtein("rosettacode", "raisethysword")
console.log levenshtein("stop", "tops")
console.log levenshtein("yo", "")
console.log levenshtein("", "yo")
