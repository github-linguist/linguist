Msgbox % IntervalAdd(1,2) ; [2.999999,3.000001]

SetFormat, FloatFast, 0.20
Msgbox % IntervalAdd(1,2) ; [2.99999999999999910000,3.00000000000000090000]

;In v1.0.48+, floating point variables have about 15 digits of precision internally
;unless SetFormat Float (i.e. the slow mode) is present anywhere in the script.
;In that case, the stored precision of floating point numbers is determined by A_FormatFloat.
;As there is no way for this function to know whether this is the case or not,
;it conservatively uses A_FormatFloat in all cases.
IntervalAdd(a,b){
	err:=0.1**(SubStr(A_FormatFloat,3) > 15 ? 15 : SubStr(A_FormatFloat,3))
	Return "[" a+b-err ","a+b+err "]"
}
