isEmptyString = (s) ->
  # Returns true iff s is an empty string.
  # (This returns false for non-strings as well.)
  return true if s instanceof String and s.length == 0
  s == ''

empties = ["", '', new String()]
non_empties = [new String('yo'), 'foo', {}]
console.log (isEmptyString(v) for v in empties) # [true, true, true]
console.log (isEmptyString(v) for v in non_empties) # [false, false, false]
console.log (s = '') == "" # true
console.log new String() == '' # false, due to underlying JavaScript's distinction between objects and primitives
