; Submitted by MasterFocus --- http://tiny.cc/iTunis

; [1] Generate the Hailstone Seq. for a number

List := varNum := 7 ; starting number is 7, not counting elements
While ( varNum > 1 )
  List .= ", " ( varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 ) )
MsgBox % List

; [2] Seq. for starting number 27 has 112 elements

Count := 1, List := varNum := 27 ; starting number is 27, counting elements
While ( varNum > 1 )
  Count++ , List .= ", " ( varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 ) )
MsgBox % "Sequence:`n" List "`n`nCount: " Count

; [3] Find number<100000 with longest seq. and show both values

MaxNum := Max := 0 ; reset the Maximum variables
TimesToLoop := 100000 ; limit number here is 100000
Offset := 70000 ; offset - use 0 to process from 0 to 100000
Loop, %TimesToLoop%
{
  If ( TimesToLoop < ( varNum := Index := A_Index+Offset ) )
    Break
  text := "Processing...`n-------------------`n"
  text .= "Current starting number: " Index "`n"
  text .= "Current sequence count: " Count
  text .= "`n-------------------`n"
  text .= "Maximum starting number: " MaxNum "`n"
  text .= "Maximum sequence count: " Max " <<" ; text split to avoid long code lines
  ToolTip, %text%
  Count := 1 ; going to count the elements, but no "List" required
  While ( varNum > 1 )
    Count++ , varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 )
  If ( Count > Max )
    Max := Count , MaxNum := Index ; set the new maximum values, if necessary
}
ToolTip
MsgBox % "Number: " MaxNum "`nCount: " Max
