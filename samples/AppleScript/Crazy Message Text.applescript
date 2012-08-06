(*
Crazy Message Text

Copyright 2002-2012 Apple Inc. All rights reserved.

You may incorporate this Apple sample code into your program(s) without
restriction.  This Apple sample code has been provided "AS IS" and the
responsibility for its operation is yours.  You are not permitted to
redistribute this Apple sample code as "Apple sample code" after having
made changes.  If you're going to redistribute the code, we require
that you make it clear that the code was descended from Apple sample
code, but that you've made changes.
*)

(*
This script takes a string from the user and then makes a new message
where each letter has a different font, size, and color.
*)

property lowFontSize : 36
property highFontSize : 72
property messageText : "Happy Birthday!"

repeat
	set userInput to display dialog "Enter some message text:" & return & return & "Minimum Character Size: " & (lowFontSize as string) & return & "Maximum Character Size: " & (highFontSize as string) default answer messageText buttons {"Cancel", "Set Prefs", "Continue"} default button 3
	
	if the button returned of userInput is "Set Prefs" then
		set minimumFontSize to 9
		
		display dialog "Enter the minimum font size to use:" & return & return & "(Must be at least " & (minimumFontSize as string) & ")" default answer lowFontSize buttons {"OK"}
		set newFontSize to text returned of the result as integer
		if newFontSize is greater than or equal to minimumFontSize then
			set lowFontSize to newFontSize
		else
			set lowFontSize to minimumFontSize
		end if
		
		display dialog "Enter the maximum font size to use:" & return & return & "(Must be greater than " & (lowFontSize as string) & ")" default answer highFontSize buttons {"OK"}
		set newFontSize to text returned of the result as integer
		if newFontSize is greater than lowFontSize then
			set highFontSize to newFontSize
		else
			set highFontSize to lowFontSize
		end if
		
	else -- button returned of userInput is "Continue"
		set theText to text returned of userInput
		if theText is not "" then
			set messageText to theText
		end if
		exit repeat
	end if
end repeat

set fontList to {"American Typewriter", "American Typewriter Light", "American Typewriter Bold", "American Typewriter Condensed", "American Typewriter Condensed Light", "American Typewriter Condensed Bold", "Arial", "Arial Italic", "Arial Bold", "Arial Bold Italic", "Arial Black", "Baskerville", "Baskerville Italic", "Baskerville SemiBold", "Baskerville Bold", "Baskerville SemiBold Italic", "Baskerville Bold Italic", "Big Caslon Medium", "Comic Sans MS", "Comic Sans MS Bold", "Copperplate", "Copperplate Light", "Copperplate Bold", "Didot", "Didot Italic", "Didot Bold", "Futura Medium", "Futura Medium Italic", "Futura Condensed Medium", "Futura Condensed ExtraBold", "Geneva", "Gill Sans", "Gill Sans Italic", "Gill Sans Light", "Gill Sans Light Italic", "Gill Sans Bold", "Gill Sans Bold Italic", "Herculanum", "Lucida Grande", "Lucida Grande Bold", "Marker Felt Thin", "Marker Felt Wide", "Optima Regular", "Optima Italic", "Optima Bold", "Optima Bold Italic", "Optima ExtraBlack", "Papyrus", "Verdana", "Verdana Italic", "Verdana Bold", "Verdana Bold Italic", "Zapfino"}

tell application "Mail"
	activate
	set crazyTextMessage to make new outgoing message with properties {content:messageText, visible:true}
	
	tell crazyTextMessage
		repeat with eachCharacter in characters
			set font of eachCharacter to (some item of fontList)
			set size of eachCharacter to (random number from lowFontSize to highFontSize)
			set color of eachCharacter to {random number from 0 to 65535, random number from 0 to 65535, random number from 0 to 65535}
		end repeat
	end tell
end tell