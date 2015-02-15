Procedure LevenshteinDistance(A_string$, B_String$)
  Protected m, n, i, j, min, k, l
  m = Len(A_string$)
  n = Len(B_String$)
  Dim D(m, n)

  For i=0 To m: D(i,0)=i: Next
  For j=0 To n: D(0,j)=j: Next

  For j=1 To n
    For i=1 To m
      If Mid(A_string$,i,1) = Mid(B_String$,j,1)
        D(i,j) = D(i-1, j-1); no operation required
      Else
        min = D(i-1, j)+1   ; a deletion
        k   = D(i, j-1)+1   ; an insertion
        l   = D(i-1, j-1)+1 ; a substitution
        If k < min: min=k: EndIf
        If l < min: min=l: EndIf
        D(i,j) = min
      EndIf
    Next
  Next
  ProcedureReturn D(m,n)
EndProcedure

;- Testing
n = LevenshteinDistance("kitten", "sitting")
MessageRequester("Info","Levenshtein Distance= "+Str(n))
