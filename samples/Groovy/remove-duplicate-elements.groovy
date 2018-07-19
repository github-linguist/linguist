def list = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
assert list.size() == 12
println "   Original List: ${list}"

// Filtering the List
list.unique()
assert list.size() == 8
println "   Filtered List: ${list}"

list = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
assert list.size() == 12

// Converting to Set
def set = new HashSet(list)
assert set.size() == 8
println "             Set: ${set}"

// Converting to Order-preserving Set
set = new LinkedHashSet(list)
assert set.size() == 8
println "List-ordered Set: ${set}"
