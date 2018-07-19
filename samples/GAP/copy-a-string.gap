#In GAP strings are lists of characters. An affectation simply copy references
a := "more";
b := a;
b{[1..4]} := "less";
a;
# "less"

# Here is a true copy
a := "more";
b := ShallowCopy(a);
b{[1..4]} := "less";
a;
# "more"
