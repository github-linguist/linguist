; The array Frame1%x%_%y% holds the current frame. frame2%x%_%y%
; is then calculated from this, and printed. frame2 is then copied to frame1.
; Two arrays are necessary so that each cell advances at the same time
; T=Tree, #=Fire, O=Empty cell
; Size holds the width and height of the map and is used as the # of iterations in loops
; This will save the map as forest_fire.txt in its working directory
; ======================================================================================


Size := 10
Generation := 0
Tree := "T"
Fire := "#"
Cell := "O"


; --Define probabilities--
    New_Tree := 5
    ; 20 percent chance (1 in 5). A random number will be generated from 1 to New_tree. If this number is 1,
    ; A tree will be created in the current cell

    Spontaneous := 10
    ; 10 percent chance (1 in 10). A random number will be generated from 1 to Spontaneous. If this number is 1,
    ; and the current cell contains a tree, the tree in the current cell will become fire.


GoSub, Generate

; ----------------------Main Loop------------------------------
loop
{
    Generation++
    GoSub, Calculate
    GoSub, Copy
    GoSub, Display
    msgbox, 4, Forest Fire, At Generation %generation%. Continue?
    IfMsgbox, No
        ExitApp
}
return
; -------------------------------------------------------------

Generate:      ; Randomly initializes the map.
loop % size    ; % forces expression mode.
{
	x := A_Index
	Loop % size
	{
		Y := A_Index
		Random, IsTree, 1, 2 ;         -- Roughly half of the spaces will contain trees
		If ( IsTree = 1 )
			Frame1%x%_%y% := Tree
		Else
			Frame1%x%_%y% := Cell
	}
}
return

Calculate:
Loop % size
{
	x := A_Index
	Loop % size
	{
		Y := A_Index
		If ( Frame1%x%_%y% = Cell )
		{
			Random, tmp, 1, New_Tree
			If ( tmp = 1 )
				Frame2%x%_%y% := tree
			Else
				Frame2%x%_%y% := Cell
		}
		Else If ( Frame1%x%_%y% = Tree )
		{
			BoolCatch := PredictFire(x,y)
			If (BoolCatch)
				Frame2%x%_%y% := Fire
			Else
				Frame2%x%_%y% := Tree
		}
		Else If ( Frame1%x%_%y% = Fire )
			Frame2%x%_%y% := Cell
		Else
		{
			contents := Frame1%x%_%y%
			Msgbox Error! Cell %x% , %y% contains %contents% ; This has never happened
			ExitApp
		}
	}
}
return

Copy:
Loop % size
{
	x := A_Index
	Loop % size
	{
		y := A_Index
		frame1%x%_%y% := Frame2%x%_%y%
	}
}
return


Display:
ToPrint := ""
ToPrint .= "=====Generation " . Generation . "=====`n"
Loop % size
{
	x := A_Index
	Loop % size
	{
		y := A_Index
		ToPrint .= Frame1%x%_%y%
	}
	ToPrint .= "`n"
}
FileAppend, %ToPrint%, Forest_Fire.txt
Return


PredictFire(p_x,p_y){
    Global ; allows access to all frame1*_* variables (the pseudo-array)
    A := p_x-1
    B := p_y-1
    C := p_x+1
    D := p_y+1
    If ( Frame1%A%_%p_Y% = fire )
        return 1
    If ( Frame1%p_X%_%B% = fire )
        return 1
    If ( Frame1%C%_%p_Y% = fire )
        return 1
    If ( Frame1%p_X%_%D% = fire )
        return 1

    If ( Frame1%A%_%B% = Fire )
        return 1
    If ( Frame1%A%_%D% = fire )
        return 1
    If ( Frame1%C%_%B% = fire )
        return 1
    If ( Frame1%C%_%D% = Fire )
        return 1

    Random, tmp, 1, spontaneous
    if ( tmp = 1 )
        return 1
    return 0
}
