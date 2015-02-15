>> require 'set'
=> true
>> s1, s2 = Set[1, 2, 3, 4], [3, 4, 5, 6].to_set # different ways of creating a set
=> [#<Set: {1, 2, 3, 4}>, #<Set: {5, 6, 3, 4}>]
>> s1 | s2 # Union
=> #<Set: {5, 6, 1, 2, 3, 4}>
>> s1 & s2 # Intersection
=> #<Set: {3, 4}>
>> s1 - s2 # Difference
=> #<Set: {1, 2}>
>> s1.proper_subset?(s1) # Proper subset
=> false
>> Set[3, 1].proper_subset?(s1) # Proper subset
=> true
>> s1.subset?(s1) # Subset
=> true
>> Set[3, 1].subset?(s1) # Subset
=> true
>> Set[3, 2, 4, 1] == s1 # Equality
=> true
>> s1 == s2 # Equality
=> false
>> s1.include?(2) # Membership
=> true
>> Set[1, 2, 3, 4, 5].proper_superset?(s1) # Proper superset
=> true
>> Set[1, 2, 3, 4].proper_superset?(s1) # Proper superset
=> false
>> Set[1, 2, 3, 4].superset?(s1) # Superset
=> true
>> s1 ^ s2 # Symmetric difference
=> #<Set: {5, 6, 1, 2}>
>> s1.size # Cardinality
=> 4
>> s1 << 99 # Mutability (or s1.add(99) )
=> #<Set: {99, 1, 2, 3, 4}>
>> s1.delete(99) # Mutability
=> #<Set: {1, 2, 3, 4}>
>> s1.merge(s2) # Mutability
=> #<Set: {5, 6, 1, 2, 3, 4}>
>> s1.subtract(s2) # Mutability
=> #<Set: {1, 2}>
>>
