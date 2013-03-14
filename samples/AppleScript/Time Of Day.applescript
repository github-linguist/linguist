(* 
 Speaks the  date and time of day
 
 Copyright 2008 Apple Inc. All rights reserved.
 
 You may incorporate this Apple sample code into your program(s) without
 restriction.  This Apple sample code has been provided "AS IS" and the
 responsibility for its operation is yours.  You are not permitted to
 redistribute this Apple sample code as "Apple sample code" after having
 made changes.  If you're going to redistribute the code, we require
 that you make it clear that the code was descended from Apple sample
 code, but that you've made changes.
 *)

on isVoiceOverRunning()
	set isRunning to false
	tell application "System Events"
		set isRunning to (name of processes) contains "VoiceOver"
	end tell
	return isRunning
end isVoiceOverRunning

on isVoiceOverRunningWithAppleScript()
	if isVoiceOverRunning() then
		set isRunningWithAppleScript to true
		
		-- is AppleScript enabled on VoiceOver --
		tell application "VoiceOver"
			try
				set x to bounds of vo cursor
			on error
				set isRunningWithAppleScript to false
			end try
		end tell
		return isRunningWithAppleScript
	end if
	return false
end isVoiceOverRunningWithAppleScript

set currentDate to current date
set amPM to "AM"
set currentHour to (currentDate's hours)
set currentMinutes to currentDate's minutes

if (currentHour > 12 and currentHour < 24) then
	set amPM to "PM"
else
	set amPM to "AM"
end if

--  make minutes below 10 sound nice
if currentMinutes < 10 then
	set currentMinutes to ("0" & currentMinutes) as text
end if

--  ensure 0:nn gets set to 12:nn AM
if currentHour is equal to 0 then
	set currentHour to 12
end if

--  readjust for 12 hour time
if (currentHour > 12) then
	set currentHour to (currentHour - 12)
end if

set currentTime to ((currentDate's month) as text) & " " & ((currentDate's day) as text) & ", " & (currentHour as text) & ":" & ((currentMinutes) as text) & " " & amPM as text

if isVoiceOverRunningWithAppleScript() then
	tell application "VoiceOver"
		output currentTime
	end tell
else
	say currentTime
	delay 2
end if
