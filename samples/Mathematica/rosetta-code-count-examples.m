TaskList = Flatten[
   Import["http://rosettacode.org/wiki/Category:Programming_Tasks", "Data"][[1, 1]]];

Print["Task \"", StringReplace[#, "_" -> " "], "\" has ",
  Length@Select[Import["http://rosettacode.org/wiki/" <> #, "Data"][[1,2]],
  StringFreeQ[#, __ ~~ "Programming Task" | __ ~~ "Omit"]& ], " example(s)"]&
  ~Map~ StringReplace[TaskList, " " -> "_"]
