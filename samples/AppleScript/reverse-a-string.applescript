get reverse_string("as⃝df̅")

on reverse_string(str)
	set old_delim to (get AppleScript's text item delimiters)
	set AppleScript's text item delimiters to ""
	
	set temp to (reverse of text items of str)
	set temp to (text items of temp) as Unicode text
	
	set AppleScript's text item delimiters to old_delim
	return temp
end reverse_string
