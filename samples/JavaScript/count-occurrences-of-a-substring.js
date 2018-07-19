function countSubstring(str, subStr){
	return str.match(new RegExp(subStr, "g")).length
}
