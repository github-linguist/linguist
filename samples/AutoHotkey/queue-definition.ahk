push("qu", 2), push("qu", 44), push("qu", "xyz") ; TEST

MsgBox % "Len = " len("qu") ; Number of entries
While !empty("qu")          ; Repeat until queue is not empty
    MsgBox % pop("qu")      ; Print popped values (2, 44, xyz)
MsgBox Error = %ErrorLevel% ; ErrorLevel =  0: OK
MsgBox % pop("qu")          ; Empty
MsgBox Error = %ErrorLevel% ; ErrorLevel = -1: popped too much
MsgBox % "Len = " len("qu") ; Number of entries

push(queue,_) {             ; push _ onto queue named "queue" (!=_), _ string not containing |
    Global
    %queue% .= %queue% = "" ? _ : "|" _
}

pop(queue) {                ; pop value from queue named "queue" (!=_,_1,_2)
    Global
    RegExMatch(%queue%, "([^\|]*)\|?(.*)", _)
    Return _1, ErrorLevel := -(%queue%=""), %queue% := _2
}

empty(queue) {              ; check if queue named "queue" is empty
    Global
    Return %queue% = ""
}

len(queue) {                ; number of entries in "queue"
    Global
    StringReplace %queue%, %queue%, |, |, UseErrorLevel
    Return %queue% = "" ? 0 : ErrorLevel+1
}
