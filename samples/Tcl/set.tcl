package require struct::set

# Many ways to build sets
set s1 [list 1 2 3 4]
set s2 {3 4 5 6}
struct::set add s3 {2 3 4 3 2};   # $s3 will be proper set...
set item 5

puts "union: [struct::set union $s1 $s2]"
puts "intersection: [struct::set intersect $s1 $s2]"
puts "difference: [struct::set difference $s1 $s2]"
puts "membership predicate: [struct::set contains $s1 $item]"
puts "subset predicate: [struct::set subsetof $s1 $s2]";   # NB: not strict subset test!
puts "equality predicate: [struct::set equal $s1 $s2]"

# Adding an element to a set (note that we pass in the name of the variable holding the set):
struct::set include s3 $item
# Removing an element from a set:
struct::set exclude s3 $item
# Getting the cardinality:
puts "cardinality: [struct::set size $s3]
