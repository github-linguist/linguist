set codeString to text returned of (display dialog "Enter BF code:" buttons "OK" default answer "")
set inputString to text returned of (display dialog "Enter input string" buttons "OK" default answer "")
set codePointer to 1
set loopPosns to {}
set tape to {}
set tapePointer to 1
set output to {}
set inputPointer to 1
set step to 0

set thePath to (path to desktop as Unicode text) & "log.txt"
set debug to (open for access file thePath with write permission)

write (step as string) & " (" & ((codePointer - 1) as string) & "): (The program contains " & ((length of codeString) as string) & " instructions.)
" to debug

set step to 1

on betterMod(x, y) -- so -2 mod 256 is 254 instead of -2
	local x
	local y
	try
		return -y * (round (x / y) rounding down) + x
	on error eMsg number eNum
		error "Can't call betterMod() on " & eMsg number eNum
	end try
end betterMod

repeat while codePointer ≤ length of codeString
	set theChar to (get character codePointer of codeString)
	
	if (theChar = "+") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		set item tapePointer of tape to betterMod(((get item tapePointer of tape) + 1), 256)
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | a[" & ((tapePointer - 1) as string) & "]= " & ((item tapePointer of tape) as string) & "
" to debug
	else if (theChar = "-") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		set item tapePointer of tape to betterMod(((get item tapePointer of tape) - 1), 256)
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | a[" & ((tapePointer - 1) as string) & "]= " & ((item tapePointer of tape) as string) & "
" to debug
	else if (theChar = "<") then
		set tapePointer to tapePointer - 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | array pos. now " & ((tapePointer - 1) as string) & "
" to debug
		
	else if (theChar = ">") then
		set tapePointer to tapePointer + 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | array pos. now " & ((tapePointer - 1) as string) & "
" to debug
		
	else if (theChar = "[") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Array[" & ((tapePointer - 1) as string) & "] is '" & ((item tapePointer of tape) as string) & "'" to debug
		if (item tapePointer of tape ≠ 0) then
			set loopPosns to loopPosns & codePointer
			write " ** Loop nesting level: " & (((length of loopPosns) - 1) as string) & ".
" to debug
		else
			write "
" & (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Not entering a loop but skipping to instruction number " to debug
			set matchLoops to 1
			repeat while matchLoops ≠ 0
				set codePointer to codePointer + 1
				if (item codePointer of codeString = "[") then
					set matchLoops to matchLoops + 1
				else if (item codePointer of codeString = "]") then
					set matchLoops to matchLoops - 1
				end if
			end repeat
			write ((codePointer - 1) as string) & "
" to debug
		end if
		
	else if (theChar = "]") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | Array[" & ((tapePointer - 1) as string) & "] is '" & ((item tapePointer of tape) as string) & "'
" to debug
		if (item tapePointer of tape ≠ 0) then
			write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | looping back to " & (((item (length of loopPosns) of loopPosns) - 1) as string) & "
" to debug
			set codePointer to (item (length of loopPosns) of loopPosns) - 1
		end if
		if (length of loopPosns > 1) then
			set loopPosns to items 1 thru ((length of loopPosns) - 1) of loopPosns
		else
			set loopPosns to {}
		end if
		
	else if (theChar = ".") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | output '" & ((item tapePointer of tape) as string) & "' " & string id (item tapePointer of tape) & "
" to debug
		set output to output & item tapePointer of tape
		
	else if (theChar = ",") then
		repeat while (length of tape < tapePointer)
			set tape to tape & 0
		end repeat
		if (inputPointer > length of inputString) then
			set inputPointer to 1
		end if
		set item tapePointer of tape to id of item inputPointer of inputString
		set inputPointer to inputPointer + 1
		write (step as string) & " (" & ((codePointer - 1) as string) & "): " & (item codePointer of codeString) & " | read in " & string id (item tapePointer of tape) & " (" & ((item tapePointer of tape) as string) & ")
" to debug
	end if
	
	set codePointer to codePointer + 1
	set step to step + 1
end repeat

set strout to string id output
display dialog strout
close access debug
