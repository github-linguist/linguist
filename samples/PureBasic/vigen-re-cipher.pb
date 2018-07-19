Procedure prepString(text.s, Array letters(1))
  ;convert characters to an ordinal (0-25) and remove non-alphabetic characters,
  ;returns dimension size of result array letters()
  Protected *letter.Character, index
  Dim letters(Len(text))
  text = UCase(text)
  *letter = @text
  While *letter\c
    Select *letter\c
      Case 'A' To 'Z'
        letters(index) = *letter\c - 65
        index + 1
    EndSelect
    *letter + SizeOf(Character)
  Wend
  If index > 0
    Redim letters(index - 1)
  EndIf
  ProcedureReturn index - 1
EndProcedure

Procedure.s VC_encrypt(text.s, keyText.s, reverse = 0)
  ;if reverse <> 0 then reverse the key (decrypt)
  Protected *letter.Character
  Dim text(0)
  Dim keyText(0)
  If prepString(text, text()) < 0 Or prepString(keyText, keyText()) < 0: ProcedureReturn: EndIf ;exit, nothing to work with

  Protected i, keyLength = ArraySize(keyText())
  If reverse
    For i = 0 To keyLength
      keyText(i) = 26 - keyText(i)
    Next
  EndIf

  Protected textLength = ArraySize(text()) ;zero-based length
  Protected result.s = Space(textLength + 1), *resultLetter.Character
  keyLength + 1 ;convert from zero-based to one-based count
  *resultLetter = @result
  For i = 0 To textLength
    *resultLetter\c = ((text(i) + keyText(i % keyLength)) % 26) + 65
    *resultLetter + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

Procedure.s VC_decrypt(cypherText.s, keyText.s)
  ProcedureReturn VC_encrypt(cypherText, keyText.s, 1)
EndProcedure

If OpenConsole()
  Define VignereCipher.s, plainText.s, encryptedText.s, decryptedText.s

  VignereCipher.s = "VIGNERECIPHER"
  plainText = "The quick brown fox jumped over the lazy dogs.": PrintN(RSet("Plain text = ", 17) + #DQUOTE$ + plainText + #DQUOTE$)
  encryptedText = VC_encrypt(plainText, VignereCipher): PrintN(RSet("Encrypted text = ", 17) + #DQUOTE$ + encryptedText + #DQUOTE$)
  decryptedText = VC_decrypt(encryptedText, VignereCipher): PrintN(RSet("Decrypted text = ", 17) + #DQUOTE$ + decryptedText + #DQUOTE$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
