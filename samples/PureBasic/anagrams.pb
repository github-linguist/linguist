InitNetwork()  ;
OpenConsole()

Procedure.s sortWord(word$)
  len.i = Len(word$)
  Dim CharArray.s (len)

  For n = 1 To len                                 ; Transfering each single character
     CharArray(n) = Mid(word$, n, 1)      ; of the word into an array.
  Next

  SortArray(CharArray(),#PB_Sort_NoCase ) ; Sorting the array.

  word$ =""
  For n = 1 To len                       ; Writing back each single
     word$ + CharArray(n)             ; character of the array.
  Next

  ProcedureReturn word$
EndProcedure

;for a faster and more advanced alternative replace the previous procedure with this code
; Procedure.s sortWord(word$) ;returns a string with the letters of the word sorted
;   Protected wordLength = Len(word$)
;   Protected Dim letters.c(wordLength)
;
;   PokeS(@letters(), word$) ;overwrite the array with the strings contents
;   SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
;   ProcedureReturn PeekS(@letters(), wordLength) ;return the arrays contents
; EndProcedure


tmpdir$   = GetTemporaryDirectory()
filename$ = tmpdir$ + "unixdict.txt"
Structure ana
   isana.l
   anas.s
EndStructure

NewMap anaMap.ana()

If ReceiveHTTPFile("http://www.puzzlers.org/pub/wordlists/unixdict.txt", filename$)
  If ReadFile(1, filename$)
    Repeat
      word$ = (ReadString(1))             ; Reading a word from a file.
      key$  = (sortWord(word$))             ; Sorting the word and storing in key$.

      If FindMapElement(anaMap(), key$)   ; Looking up if a word already had the same key$.

                                          ; if yes
         anaMap()\anas  = anaMap()\anas+ ", " + word$   ; adding the word
         anaMap()\isana + 1
      Else
                                          ; if no
         anaMap(key$)\anas = word$        ; applying  a new record
         anaMap()\isana = 1
       EndIf

      If anaMap()\isana > maxAnagrams ;make note of maximum anagram count
        maxAnagrams = anaMap()\isana
      EndIf

    Until Eof(1)
    CloseFile(1)
    DeleteFile(filename$)

    ;----- output -----
    ForEach anaMap()
      If anaMap()\isana = maxAnagrams      ; only emit elements that have the most hits
        PrintN(anaMap()\anas)
      EndIf
    Next

    PrintN("Press any key"): Repeat: Until Inkey() <> ""
  EndIf
EndIf
