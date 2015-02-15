# For ad-hoc set features, it sometimes makes sense to use hashes directly,
# rather than abstract to this level, but I'm showing a somewhat heavy
# solution to show off CoffeeScript class syntax.
class Set
  constructor: (elems...) ->
    @hash = {}
    for elem in elems
      @hash[elem] = true

  add: (elem) ->
    @hash[elem] = true

  remove: (elem) ->
    delete @hash[elem]

  has: (elem) ->
    @hash[elem]?

  union: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem
    for elem in set2.to_array()
      set.add elem
    set

  intersection: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem if set2.has elem
    set

  minus: (set2) ->
    set = new Set()
    for elem of @hash
      set.add elem if !set2.has elem
    set

  is_subset_of: (set2) ->
    for elem of @hash
      return false if !set2.has elem
    true

  equals: (set2) ->
    this.is_subset_of(set2) and set2.is_subset_of this

  to_array: ->
    (elem for elem of @hash)

  each: (f) ->
    for elem of @hash
      f(elem)

  to_string: ->
    @to_array()

run_tests = ->
  set1 = new Set("apple", "banana") # creation
  console.log set1.has "apple" # true (membership)
  console.log set1.has "worms" # false (membership)

  set2 = new Set("banana", "carrots")
  console.log set1.union(set2).to_string() # [ 'apple', 'banana', 'carrots' ] (union)
  console.log set1.intersection(set2).to_string() # [ 'banana' ] (intersection)
  console.log set1.minus(set2).to_string() # [ 'apple' ] (difference)

  set3 = new Set("apple")
  console.log set3.is_subset_of set1 # true
  console.log set3.is_subset_of set2 # false

  set4 = new Set("apple", "banana")
  console.log set4.equals set1 # true
  console.log set4.equals set2 # false

  set5 = new Set("foo")
  set5.add "bar" # add
  console.log set5.to_string() # [ 'foo', 'bar' ]
  set5.remove "bar" # remove
  console.log set5.to_string() # [ 'foo' ]

  # iteration, prints apple then banana (order not guaranteed)
  set1.each (elem) ->
    console.log elem

run_tests()
