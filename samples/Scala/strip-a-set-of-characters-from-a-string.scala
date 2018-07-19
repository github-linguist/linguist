def stripChars(s:String, ch:String)= s filterNot (ch contains _)

stripChars("She was a soul stripper. She took my heart!", "aei")
// => Sh ws  soul strppr. Sh took my hrt!
