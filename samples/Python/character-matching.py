"abcd".startswith("ab") #returns True
"abcd".endswith("zn") #returns False
"bb" in "abab" #returns False
"ab" in "abab" #returns True
loc = "abab".find("bb") #returns -1
loc = "abab".find("ab") #returns 0
loc = "abab".find("ab",loc+1) #returns 2
