begins_with("abcd","ab") #returns true
search("abcd","ab") #returns 1:2, indices range where string was found
ends_with("abcd","zn") #returns false
ismatch(r"ab","abcd") #returns true where 1st arg is regex string
julia>for r in each_match(r"ab","abab")
	println(r.offset)
end
1
3
