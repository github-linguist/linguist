Key = VIGENERECIPHER
Text= Beware the Jabberwock, my son! The jaws that bite, the claws that catch!

out := "Input      =" text "`nkey        =" key "`nCiphertext =" (c := VigenereCipher(Text, Key)) "`nDecrypted  =" VigenereDecipher(c, key)
MsgBox % clipboard := out

VigenereCipher(Text, Key){
   StringUpper, Text, Text
   Text := RegExReplace(Text, "[^A-Z]")
   Loop Parse, Text
   {
      a   := Asc(A_LoopField)                                   - Asc("A")
      b   := Asc(SubStr(Key, 1+Mod(A_Index-1, StrLen(Key)), 1)) - Asc("A")
      out .= Chr(Mod(a+b,26)+Asc("A"))
   }
   return out
}

VigenereDecipher(Text, key){
   Loop Parse, key
      decoderKey .= Chr(26-(Asc(A_LoopField)-65)+65)
   return VigenereCipher(Text, decoderKey)
}
