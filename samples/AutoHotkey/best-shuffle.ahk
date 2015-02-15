words := "abracadabra,seesaw,elk,grrrrrr,up,a"
Loop Parse, Words,`,
   out .= Score(A_LoopField, Shuffle(A_LoopField))
MsgBox % clipboard := out


Shuffle(String)
{
 Cord := String
 Length := StrLen(String)
 CharType := A_IsUnicode ? "UShort" : "UChar"

 Loop, Parse, String  ; For each old character in String...
 {
  Char1 := SubStr(Cord, A_Index, 1)
  If (Char1 <> A_LoopField)  ; If new character already differs,
   Continue                  ;  do nothing.

  Index1 := A_Index
  OldChar1 := A_LoopField
  Random, Index2, 1, Length  ; Starting at some random index,
  Loop, %Length%             ;  for each index...
  {
   If (Index1 <> Index2)     ; Swap requires two different indexes.
   {
    Char2 := SubStr(Cord, Index2, 1)
    OldChar2 := SubStr(String, Index2, 1)

    ; If after the swap, the two new characters would differ from
    ; the two old characters, then do the swap.
    If (Char1 <> OldChar2) and (Char2 <> OldChar1)
    {
     ; Swap Char1 and Char2 inside Cord.
     NumPut(Asc(Char1), Cord, (Index2 - 1) << !!A_IsUnicode, CharType)
     NumPut(Asc(Char2), Cord, (Index1 - 1) << !!A_IsUnicode, CharType)
     Break
   }
   }
   Index2 += 1           ; Get next index.
   If (Index2 > Length)  ; If after last index,
    Index2 := 1          ;  use first index.
  }
 }
 Return Cord
}
Score(a, b){
	r := 0
	Loop Parse, a
		If (A_LoopField = SubStr(b, A_Index, 1))
			r++
	return a ", " b ", (" r ")`n"
}
