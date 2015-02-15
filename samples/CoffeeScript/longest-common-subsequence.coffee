lcs = (s1, s2) ->
  len1 = s1.length
  len2 = s2.length

  # Create a virtual matrix that is (len1 + 1) by (len2 + 1),
  # where m[i][j] is the longest common string using only
  # the first i chars of s1 and first j chars of s2.  The
  # matrix is virtual, because we only keep the last two rows
  # in memory.
  prior_row = ('' for i in [0..len2])

  for i in [0...len1]
    row = ['']
    for j in [0...len2]
      if s1[i] == s2[j]
        row.push prior_row[j] + s1[i]
      else
        subs1 = row[j]
        subs2 = prior_row[j+1]
        if subs1.length > subs2.length
          row.push subs1
        else
          row.push subs2
    prior_row = row

  row[len2]

s1 = "thisisatest"
s2 = "testing123testing"
console.log lcs(s1, s2)
