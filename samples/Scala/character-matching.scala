"abcd".startsWith("ab") //returns true
"abcd".endsWith("zn") //returns false
"abab".contains("bb") //returns false
"abab".contains("ab") //returns true
		
var loc="abab".indexOf("bb") //returns -1
loc = "abab".indexOf("ab") //returns 0
loc = "abab".indexOf("ab", loc+1) //returns 2
