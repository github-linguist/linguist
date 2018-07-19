stripchars[a_,b_]:=StringReplace[a,(#->"")&/@Characters[b]]
stripchars["She was a soul stripper. She took my heart!","aei"]
->Sh ws  soul strppr. Sh took my hrt!
