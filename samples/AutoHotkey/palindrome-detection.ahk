IsPalindrome(Str){
	Loop, Parse, Str
		ReversedStr := A_LoopField . ReversedStr
	return, (ReversedStr == Str)?"Exact":(RegExReplace(ReversedStr,"\W")=RegExReplace(Str,"\W"))?"Inexact":"False"
}
