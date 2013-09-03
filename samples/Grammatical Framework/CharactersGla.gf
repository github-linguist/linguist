resource CharactersGla = {
	
	--Character classes
	oper
		vowel : pattern Str = #("a"|"e"|"i"|"o"|"u"|"à"|"è"|"ì"|"ò"|"ù") ;
		vowelCap : pattern Str = #("A"|"E"|"I"|"O"|"U"|"À"|"É"|"Ì"|"Ò"|"Ù") ;
		consonant : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"z") ;
		consonantCap : pattern Str = #("B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Z") ;
		broadVowel : pattern Str = #("a"|"o"|"u"|"à"|"ò"|"ù") ;
		slenderVowel : pattern Str = #("e"|"i"|"è"|"ì") ;
	
}