; Given a complete list of ranked preferences, where the most liked is to the left:
abe := ["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"]
bob := ["cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"]
col := ["hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"]
dan := ["ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"]
ed := ["jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"]
fred := ["bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"]
gav := ["gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"]
hal := ["abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"]
ian := ["hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"]
jon := ["abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"]
abi := ["bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"]
bea := ["bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"]
cath := ["fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"]
dee := ["fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"]
eve := ["jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"]
fay := ["bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"]
gay := ["jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"]
hope := ["gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"]
ivy := ["ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"]
jan := ["ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"]

; of ten males:
males := ["abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon"]

; and ten females:
females := ["abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"]

; and an empty set of engagements:
engagements := Object()
freemales := males.Clone()
,s := "Engagements:`n"

; use the Gale Shapley algorithm to find a stable set of engagements:
For i, male in freemales ; i=index of male (not needed)
{
	j:=1 ; index of female
	While (engagements[female:=%male%[j]] != "" and index(%female%, male) > index(%female%, engagements[female]))
		j++ ; each male loops through all females in order of his preference until one accepts him
	If (engagements[female] != "") ; if she was previously engaged
		freemales.insert(engagements[female]) ; her old male goes to the bottom of the list
		,s .= female . " dumped " . engagements[female] . "`n"
	engagements[female] := male ; the new engagement is registered
	,s .= female . " accepted " . male . "`n"
}

; summarize results:
s .= "`nCouples:`n"
For female, male in engagements
	s .= female . " is engaged to " . male . "`n"
s .= Stable(engagements, females)

; then perturb this set of engagements to form an unstable set of engagements then check this new set for stability:
s .= "`nWhat if cath and ivy swap?`n"
engagements["cath"]:="abe", engagements["ivy"]:="bob"
	
; summarize results:
s .= "`nCouples:`n"
For female, male in engagements
	s .= female . " is engaged to " . male . "`n"
s .= Stable(engagements, females)

Msgbox % clipboard := s
Return

; Functions:
Index(obj, value) {
	For key, val in obj
		If (val = value)
			Return, key, ErrorLevel := 0
	Return, False, Errorlevel := 1
}

Stable(engagements, females) {
	For female, male in engagements
	{
		For j, female2 in females ; j=index of female (not needed)
		{
			If (index(%male%, female) > index(%male%, female2)
				and index(%female2%, male2:=engagements[female2]) > index(%female2%, male))
				s .= male . " is engaged to " . female . " but would prefer " . female2
					. " and " . female2 . " is engaged to " . male2 . " but would prefer " . male . "`n"
		}
	}
	If s
		Return "`nThese couples are not stable.`n" . s
	Else
		Return "`nThese couples are stable.`n"
}
