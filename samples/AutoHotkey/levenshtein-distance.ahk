levenshtein(s, t){
	If s =
		return StrLen(t)
	If t =
		return strLen(s)
	If SubStr(s, 1, 1) = SubStr(t, 1, 1)
		return levenshtein(SubStr(s, 2), SubStr(t, 2))
	a := Levenshtein(SubStr(s, 2), SubStr(t, 2))
	b := Levenshtein(s,            SubStr(t, 2))
	c := Levenshtein(SubStr(s, 2), t           )
	If (a > b)
		a := b
	if (a > c)
		a := c
	return a + 1
}
s1 := "kitten"
s2 := "sitting"
MsgBox % "distance between " s1 " and " s2 ": " levenshtein(s1, s2)
