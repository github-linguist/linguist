Str := "a!===b=!=c"
Sep := ["==","!=", "="]
Res := StrSplit(Str, Sep)
for k, v in Res
	Out .= (Out?",":"")  v
MsgBox % Out
for k, v in Sep
	N .= (N?"|":"") "\Q" v "\E"
MsgBox % RegExReplace(str, "(.*?)(" N ")", "$1 {$2}")
