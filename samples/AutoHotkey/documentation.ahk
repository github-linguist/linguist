;
; Function: example_add
; Description:
;     Adds two or three numbers (see [url=http://en.wikipedia.org/Number]Number [/url])
; Syntax: example_add(number1, number2[, number3=0])
; Parameters:
;		number1 - First number to add.
;		number2 - Second number to add.
;		number3 - (Optional) Third number to add. You can just omit this parameter.
; Return Value:
;		sum of parameters
; Example:
;		MsgBox % example_add(example_add(2, 3, 4), 5)
;
example_add(number1, number2, number3=0){
    return number1 + number2 + number3
}
