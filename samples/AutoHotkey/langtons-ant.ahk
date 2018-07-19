SetBatchLines -1
n := 0, x := 50, y := 50, d := 1 ; starting positions and orientation

While x > 0 and x < 100 and y > 0 and y < 100 ; In this loop the ant moves
	 d := d + !!(a%x%_%y%) - !(a%x%_%y%)
	,d := d=5 ? 1 : d=0 ? 4 : d
	,a%x%_%y% := !a%x%_%y%
	,x := x + (d=3) - (d=1)
	,y := y + (d=4) - (d=2)

Loop 99 ; in this loop the ant's movements are compiled into a string
{
	y := A_Index
	Loop 99
		 x := A_Index
		,o .= a%x%_%y% ? "#" : "."

	o .= "`r`n"
}
clipboard := o ; set the string to the clipboard
