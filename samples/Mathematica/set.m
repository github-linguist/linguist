set1 = {"a", "b", "c", "d", "e"}; set2 = {"a", "b", "c", "d", "e", "f", "g"};
MemberQ[set1, "a"]
Union[set1 , set2]
Intersection[set1 , set2]
Complement[set2, set1](*Set Difference*)
MemberQ[Subsets[set2], set1](*Subset*)
set1 == set2(*Equality*)
set1 == set1(*Equality*)
