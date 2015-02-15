;string creation
x$ = "hello world"

;string destruction
x$ = ""

;string comparison
If x$ = "hello world" : PrintN("String is equal") : EndIf

;string copying;
y$ = x$

; check If empty
If x$ = "" : PrintN("String is empty") : EndIf

; append a byte
x$ = x$ + Chr(41)

; extract a substring
x$ = Mid(x$, 1, 5)

; replace bytes
x$ = ReplaceString(x$, "world", "earth")

; join strings
x$ = "hel" + "lo w" + "orld"
