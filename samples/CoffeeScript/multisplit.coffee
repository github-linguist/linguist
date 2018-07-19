multi_split = (text, separators) ->
  # Split text up, using separators to break up text and discarding
  # separators.
  #
  # Returns an array of strings, which can include empty strings when
  # separators are found either adjacent to each other or at the
  # beginning/end of the text.
  #
  # Separators have precedence, according to their order in the array,
  # and each separator should be at least one character long.
  result = []
  i = 0
  s = ''
  while i < text.length
    found = false
    for separator in separators
      if text.substring(i, i + separator.length) == separator
        found = true
        i += separator.length
        result.push s
        s = ''
        break
    if !found
      s += text[i]
      i += 1
  result.push s
  result

console.log multi_split 'a!===b=!=c', ['==', '!=', '='] # [ 'a', '', 'b', '', 'c' ]
console.log multi_split '', ['whatever'] # [ '' ]
