; BUGGY - FIX

#Persistent
#SingleInstance OFF
SetBatchLines, -1
SortMethods := "Bogo,Bubble,Cocktail,Counting,Gnome,Insertion,Merge,Permutation,Quick,Selection,Shell,BuiltIn"
Gui, Add, Edit, vInput, numbers,separated,by,commas,without,spaces,afterwards
Loop, PARSE, SortMethods, `,
	Gui, Add, CheckBox, v%A_LoopField%, %A_LoopField% Sort
Gui, Add, Button, gTest, Test!
Gui, Show,, SortTest!
Return
Test:
SplashTextOn,,, Test Commencing
Sleep 2500
SplashTextOff
Gui, +OwnDialogs
Gui, Submit, NoHide
Loop, PARSE, SortMethods, `,
{
	If (%A_LoopField%)
	{
		DllCall("QueryPerformanceCounter", "Int64 *", %A_LoopField%Begin)
		%A_LoopField%Out := %A_LoopField%Sort(Input)
		DllCall("QueryPerformanceCounter", "Int64 *", %A_LoopField%Time)
		%A_LoopField%End := %A_LoopField%Begin + %A_LoopField%Time
		%A_LoopField%Time -= %A_LoopField%Begin
	}
}
Time := ""
Loop, PARSE, SortMethods, `,
	If (%A_LoopField%)
		Time .= A_LoopField . " Sort: " . %A_LoopField%Time . "`t`t" . %A_LoopField%Out . "`r`n"
MsgBox,, Results!, %Time%
Return



; Sorting funtions (Bogo, Bubble, Cocktail, Counting, Gnome, Insertion, Merge, Permutation, Quick, Selection, Shell, BuiltIn):

BogoSort(var)
{
	sorted := 1
	Loop, Parse, var
	{
		current := A_LoopField
		rest := SubStr(var, A_Index)
		Loop, Parse, rest
		{
			If (current > A_LoopField)
			sorted := 0
		}
	}
	While !sorted {
		sorted := 1
		Loop, Parse, var, `,
		{
			current := A_LoopField
			rest := SubStr(var, A_Index)
			Loop, Parse, rest, `,
			{
				If (current > A_LoopField)
				sorted := 0
			}
		}

		Sort, var, D`, Random
	}
	Return var
}

BubbleSort(var)
{
	StringSplit, array, var, `,
	hasChanged = 1
	size := array0
	While hasChanged
	{
		hasChanged = 0
		Loop, % (size - 1)
		{
			i := array%A_Index%
			aj := A_Index + 1
			j := array%aj%
			If (j < i)
			{
				temp := array%A_Index%
				array%A_Index% := array%aj%
				array%aj% := temp
				hasChanged = 1
			}
		}
	}
	Loop, % size
	sorted .= "," . array%A_Index%
	Return substr(sorted,2)
}

CocktailSort(var)
{
	StringSplit array, var, `,
	i0 := 1, i1 := array0
	Loop
	{
		Changed =
		Loop % i1-- -i0 {
			j := i0+A_Index, i := j-1
			If (array%j% < array%i%)
				t := array%i%, array%i% := array%j%, array%j% := t
				,Changed = 1
		}
		IfEqual Changed,, Break
		Loop % i1-i0++
		{
			i := i1-A_Index, j := i+1
			If (array%j% < array%i%)
			t := array%i%, array%i% := array%j%, array%j% := t
			,Changed = 1
		}
		IfEqual Changed,, Break
	}
	Loop % array0
		sorted .= "," . array%A_Index%
	Return SubStr(sorted,2)
}

CountingSort(var)
{
	max := min := substr(var, 1, instr(var, ","))
	Loop, parse, var, `,
	{
		If (A_LoopField > max)
			max := A_LoopField

		Else If (A_LoopField < min)
			min := A_LoopField
	}
	Loop % max-min+1
		i := A_Index-1, a%i% := 0
	Loop, Parse, var, `,
		i := A_LoopField-min, a%i%++
	Loop % max-min+1
	{
		i := A_Index-1, v := i+min
		Loop % a%i%
			t .= "," v
	}
	Return SubStr(t,2)
}

GnomeSort(var) {
	StringSplit, a, var, `,
	i := 2, j := 3
	While i <= a0 {
		u := i-1
		If (a%u% < a%i%)
			i := j, j := j+1
		Else {
			t := a%u%, a%u% := a%i%, a%i% := t
			If (--i = 1)
				i := j, j++
		}
	}
	Loop % a0
	sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}

InsertionSort(var) {
	StringSplit, a, var, `,
	Loop % a0-1 {
		i := A_Index+1, v := a%i%, j := i-1
		While j>0 and a%j%>v
			u := j+1, a%u% := a%j%, j--
		u := j+1, a%u% := v
	}
	Loop % a0
	sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}


MergeSort(var) {
	StringReplace, t, var, `,,, UseErrorLevel
	L := ((t = "") ? 0 : ErrorLevel+1)
	If (2 > L)
		Return var
	StringGetPos, p, var, `,, % "L" L//2
	list0 := MergeSort(SubStr(var,1,p))
	list1 := MergeSort(SubStr(var,p+2))
	If (list0 = "")
		Return list1
	Else If (list1 = "")
		Return list0
	list := list0
	i0 := (p0 := InStr(list,",",0,i:=p0+1)) ? SubStr(list,i,p0-i) : SubStr(list,i)
	list := list1
	i1 := (p1 := InStr(list,",",0,i:=p1+1)) ? SubStr(list,i,p1-i) : SubStr(list,i)
	Loop  {
		i := i0>i1
		list .= "," i%i%
		If (p%i%) {
			list := list%i%
			i%i% := (p%i% := InStr(list,",",0,i:=p%i%+1)) ? SubStr(list,i,p%i%-i) : SubStr(list,i)
		}
		Else {
			i ^= 1
			rtv := SubStr(list "," i%i% (p%i% ? "," SubStr(list%i%,p%i%+1) : ""), 2)
		}
	}
	Return rtv
}

PermutationSort(var) {
	static a:="a",v:="v"
	StringSplit, a, var, `,
	v0 := a0
	Loop %v0%
		v%A_Index% := A_Index
	unsorted := 0
	Loop % %a%0-1 {
		i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
		If (%a%%i% > %a%%j%)
			unSorted := 1
	}
	While unSorted {
		i := %v%0, i1 := i-1
		While %v%%i1% >= %v%%i% {
			--i, --i1
			IfLess i1,1, Return 1
		}
		j := %v%0
		While %v%%j% <= %v%%i1%
			--j
		t := %v%%i1%, %v%%i1% := %v%%j%, %v%%j% := t,  j := %v%0
		While i < j
			t := %v%%i%, %v%%i% := %v%%j%, %v%%j% := t, ++i, --j
		unsorted := 0
		Loop % %a%0-1 {
			i := %v%%A_Index%, j := A_Index+1, j := %v%%j%
			If (%a%%i% > %a%%j%)
				unSorted := 1
		}
	}
	Loop % a0
		i := v%A_Index%, sorted .= "," . a%i%
	Return SubStr(sorted,2)
}

QuickSort(var)
{
	StringSplit, list, var, `,
	If (list0 <= 1)
		Return list
	pivot := list1
	Loop, Parse, var, `,
	{
		If (A_LoopField < pivot)
			less .= "," . A_LoopField
		Else If (A_LoopField > pivot)
			more .= "," . A_LoopField
		Else
			pivotlist .= "," . A_LoopField
	}
	less := QuickSort(substr(less,2))
	more := QuickSort(substr(more,2))
	Return substr(less,2) . pivotList . more
}

SelectionSort(var) {
	StringSplit, a, var, `,
	Loop % a0-1 {
		i := A_Index, mn := a%i%, j := m := i
		Loop % a0-i {
			j++
			If (a%j% < mn)
				mn := a%j%, m := j
		}
		t := a%i%, a%i% := a%m%, a%m% := t
	}
	Loop % a0
		sorted .= "," . a%A_Index%
	Return SubStr(sorted,2)
}

ShellSort(var) {
	StringSplit, a, var, `,
	inc := a0
	While inc:=round(inc/2.2)
		Loop % a0-inc {
			i := A_Index+inc, t := a%i%, j := i, k := j-inc
			While j > inc && a%k% > t
				a%j% := a%k%, j := k, k -= inc
			a%j% := t
		}
	Loop % a0
		s .= "," . a%A_Index%
	Return SubStr(s,2)
}

BuiltInSort(var) {
	Sort, var, N D`,
	Return var
}
