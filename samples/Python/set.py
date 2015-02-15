>>> s1, s2 = {1, 2, 3, 4}, {3, 4, 5, 6}
>>> s1 | s2; # Union
{1, 2, 3, 4, 5, 6}
>>> s1 & s2; # Intersection
{3, 4}
>>> s1 - s2; # Difference
{1, 2}
>>> s1 < s1; # True subset
False
>>> {3, 1} < s1; # True subset
True
>>> s1 <= s1; # Subset
True
>>> {3, 1} <= s1; # Subset
True
>>> {3, 2, 4, 1} == s1; # Equality
True
>>> s1 == s2; # Equality
False
>>> 2 in s1; # Membership
True
>>> 10 not in s1; # Non-membership
True
>>> {1, 2, 3, 4, 5} > s1; # True superset
True
>>> {1, 2, 3, 4} > s1; # True superset
False
>>> {1, 2, 3, 4} >= s1; # Superset
True
>>> s1 ^ s2; # Symmetric difference
{1, 2, 5, 6}
>>> len(s1); # Cardinality
4
>>> s1.add(99); # Mutability
>>> s1
{99, 1, 2, 3, 4}
>>> s1.discard(99); # Mutability
>>> s1
{1, 2, 3, 4}
>>> s1 |= s2; # Mutability
>>> s1
{1, 2, 3, 4, 5, 6}
>>> s1 -= s2; # Mutability
>>> s1
{1, 2}
>>> s1 ^= s2; # Mutability
>>> s1
{1, 2, 3, 4, 5, 6}
>>>
