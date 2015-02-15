is_pangram = (s) ->
  # This is optimized for longish strings--as soon as all 26 letters
  # are encountered, we will be done.  Our worst case scenario is a really
  # long non-pangram, or a really long pangram with at least one letter
  # only appearing toward the end of the string.
  a_code = 'a'.charCodeAt(0)
  required_letters = {}
  for i in [a_code...a_code+26]
    required_letters[String.fromCharCode(i)] = true

  cnt = 0
  for c in s
    c = c.toLowerCase()
    if required_letters[c]
      cnt += 1
      return true if cnt == 26
      delete required_letters[c]
  false

do ->
  tests = [
    ["is this a pangram", false]
    ["The quick brown fox jumps over the lazy dog", true]
  ]

  for test in tests
    [s, exp_value] = test
    throw Error("fail") if is_pangram(s) != exp_value
    # try long strings
    long_str = ''
    for i in [1..500000]
      long_str += s
    throw Error("fail") if is_pangram(long_str) != exp_value
    console.log "Passed tests: #{s}"
