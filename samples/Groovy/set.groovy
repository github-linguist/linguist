def s1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] as Set
def m1 = 6
def m2 = 7
def s2 = [0, 2, 4, 6, 8] as Set
assert m1 in s1                                        : 'member'
assert ! (m2 in s2)                                    : 'not a member'
def su = s1 + s2
assert su == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] as Set : 'union'
def si = s1.intersect(s2)
assert si == [8, 6, 4, 2] as Set                       : 'intersection'
def sd = s1 - s2
assert sd == [1, 3, 5, 7, 9, 10] as Set                : 'difference'
assert s1.containsAll(si)                              : 'subset'
assert ! s1.containsAll(s2)                            : 'not a subset'
assert (si + sd) == s1                                 : 'equality'
assert (s2 + sd) != s1                                 : 'inequality'
assert s1 != su && su.containsAll(s1)                  : 'proper subset'
s1 << 0
assert s1 == su                                        : 'added element 0 to s1'
