; http://www.autohotkey.com/forum/viewtopic.php?t=41479
inputbox, files, files, file pattern such as c:\files\*.txt

word2docs := object() ; autohotkey_L is needed.

stime := A_tickcount
Loop, %files%, 0,1
{
   tooltip,%A_index%  / 500

   wordList := WordsIn(A_LoopFileFullPath)
   InvertedIndex(wordList, A_loopFileFullpath)
}

tooltip
msgbox, % "total time " (A_tickcount-stime)/1000

gosub, search
return

search:
Loop
{
   InputBox, keyword , input single keyword only
   msgbox, % foundDocs := findword(keyword)
}
return

WordsIn(docpath)
{
   FileRead, content, %docpath%
  spos = 1
   Loop
   {
     if !(spos := Regexmatch(content, "[a-zA-Z]{2,}",match, spos))
       break
     spos += strlen(match)
     this_wordList .= match "`n"
   }

  Sort, this_wordList, U
  return this_wordList
}

InvertedIndex(byref words, docpath)
{
   global word2docs

  loop, parse, words, `n,`r
  {
    if A_loopField =
      continue
    word2docs[A_loopField] := word2docs[A_loopField] docpath "`n"
  }
}

findWord(word2find)
{
  global word2docs

  if (word2docs[word2find] = "")
     return ""
  else
    return word2docs[word2find]
}
