(*
Count Messages in All Mailboxes

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
This script goes through each mailbox, gets the total message count and
the unread count, then displays the final output in a new email message.
*)

tell application "Mail"
	set localMailboxes to every mailbox
	if (count of localMailboxes) is greater than 0 then
		set messageCountDisplay to "Local mailboxes (On My Mac)" & return & my getMessageCountsForMailboxes(localMailboxes)
	else
		set messageCountDisplay to ""
	end if
	
	set everyAccount to every account
	repeat with eachAccount in everyAccount
		set accountMailboxes to every mailbox of eachAccount
		if (count of accountMailboxes) is greater than 0 then
			set messageCountDisplay to messageCountDisplay & return & "Mailboxes for Account: " & name of eachAccount & return & my getMessageCountsForMailboxes(accountMailboxes)
		end if
	end repeat
	
	set outputMessage to make new outgoing message with properties {content:messageCountDisplay, subject:"Message counts for all my mailboxes", visible:true}
	tell outputMessage
		set font to "Courier"
		set size to 12
	end tell
end tell

on getMessageCountsForMailboxes(theMailboxes)
	-- (list of mailboxes)
	-- returns string
	
	set displayString to ""
	
	tell application "Mail"
		repeat with eachMailbox in theMailboxes
			set mailboxName to name of eachMailbox
			set messageCount to (count of (messages of eachMailbox)) as string
			set unreadCount to unread count of eachMailbox as string
			
			set displayString to displayString & "    " & my padString(mailboxName, 40) & "  " & messageCount & " (" & unreadCount & " unread)" & return
		end repeat
	end tell
	
	return displayString
end getMessageCountsForMailboxes

on padString(theString, fieldLength)
	-- (string, integer)
	-- returns string
	
	set stringLength to length of theString
	
	if stringLength is greater than fieldLength then
		set paddedString to (text from character 1 to character (fieldLength - 3) of theString) & "..."
	else -- stringLength is less than or equal to fieldLength
		set paddedString to theString
		
		set paddingLength to fieldLength - stringLength
		repeat paddingLength times
			set paddedString to paddedString & space
		end repeat
	end if
	
	return paddedString
end padString