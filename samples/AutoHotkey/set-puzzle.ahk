; Generate deck; card encoding from Perl6
Loop, 81
	deck .= ToBase(A_Index-1, 3)+1111 ","
deck := RegExReplace(deck, "3", "4")

; Shuffle
deck := shuffle(deck)

msgbox % clipboard := allValidSets(9, 4, deck)
msgbox % clipboard := allValidSets(12, 6, deck)

; Render a hand (or any list) of cards
PrettyHand(hand) {
	 Color1:="red",Color2:="green",Color4:="purple"
	,Symbl1:="oval",Symbl2:="squiggle",Symbl4:="diamond"
	,Numbr1:="one",Numbr2:="two",Numbr4:="three"
	,Shape1:="solid",Shape2:="open",Shape4:="striped"
	Loop, Parse, hand, `,
	{
		StringSplit, i, A_LoopField
		s .= "`t" Color%i1% "`t" Symbl%i2% "`t" Numbr%i3% "`t" Shape%i4% "`n"
	}
	Return s
}

; Get all unique valid sets of three cards in a hand.
allValidSets(n, m, deck) {
	While j != m
	{
		j := 0
		,hand := draw(n, deck)
		,s := "Dealt " n " cards:`n" . prettyhand(hand)
		StringSplit, set, hand, `,
		comb := comb(n,3)
		Loop, Parse, comb, `n
		{
			StringSplit, i, A_LoopField, %A_Space%
			If isValidSet(set%i1%, set%i2%, set%i3%)
				s .= "`nSet " ++j ":`n" . prettyhand(set%i1% "," set%i2% "," set%i3%)
		}
	}
	Return s
}

; Convert n to arbitrary base using recursion
toBase(n,b) {  ; n >= 0, 1 < b < StrLen(t), t = digits
	Static t := "0123456789ABCDEF"
	Return (n < b ? "" : ToBase(n//b,b)) . SubStr(t,mod(n,b)+1,1)
}

; Knuth shuffle from http://rosettacode.org/wiki/Knuth_Shuffle#AutoHotkey
shuffle(list) {									; shuffle comma separated list, converted to array
	StringSplit a, list, `,						; make array (length = a0)
	Loop % a0-1 {
		Random i, A_Index, a0					; swap item 1,2... with a random item to the right of it
		t := a%i%, a%i% := a%A_Index%, a%A_Index% := t
	}
	Loop % a0									; construct string from sorted array
		s .= "," . a%A_Index%
	Return SubStr(s,2)							; drop leading comma
}

; Randomly pick a hand of cards from the deck
draw(n, deck) {
	Loop, % n
	{
		Random, i, 1, 81
		cards := deck
		Loop, Parse, cards, `,
			(A_Index = i) ? (hand .= A_LoopField ",") : (cards .= A_LoopField ",")
		deck := cards
	}
	Return SubStr(hand, 1, -1)
}

; Test if a particular group of three cards is a valid set
isValidSet(a, b, c) {
	StringSplit, a, a
	StringSplit, b, b
	StringSplit, c, c
	Return !((a1|b1|c1 ~= "[3,5,6]") + (a2|b2|c2 ~= "[3,5,6]") + (a3|b3|c3 ~= "[3,5,6]") + (a4|b4|c4 ~= "[3,5,6]"))
}

; Get all combinations, from http://rosettacode.org/wiki/Combinations#AutoHotkey
comb(n,t) { ; Generate all n choose t combinations of 1..n, lexicographically
	IfLess n,%t%, Return
	Loop %t%
		c%A_Index% := A_Index
	i := t+1, c%i% := n+1
	
	Loop {
		Loop %t%
			i := t+1-A_Index, c .= c%i% " "
		c .= "`n"	  ; combinations in new lines
		j := 1, i := 2
		Loop
			If (c%j%+1 = c%i%)
				 c%j% := j, ++j, ++i
			Else Break
		If (j > t)
			Return c
		c%j% += 1
	}
}
