(*
Copyright 2003 Apple Computer, Inc.

You may incorporate this Apple sample code into your program(s) without
restriction.  This Apple sample code has been provided "AS IS" and the
responsibility for its operation is yours.  You are not permitted to
redistribute this Apple sample code as "Apple sample code" after having
made changes.  If you're going to redistribute the code, we require
that you make it clear that the code was descended from Apple sample
code, but that you've made changes.
*)

property type_list : {"JPEG", "GIFf", "PICT", "TIFF", "PDF", "TEXT"}
property extension_list : {"jpg", "gif", "pct", "tif", "pdf", "rtf"}
--html is not currently handled

on run {}
	tell application "Finder" to set FinderSelection to the selection as alias list
	
	set FS to FinderSelection
	--Ideally, this list could be passed to the open handler
	
	set SelectionCount to number of FS -- count	
	if SelectionCount is 0 then
		set FS to userPicksFolder()
	else if the SelectionCount is 1 then
		set MyPath to path to me
		if MyPath is item 1 of FS then
			--If I'm a droplet then I was double-clicked
			set FS to userPicksFolder()
		end if
	else
		--I'm not a double-clicked droplet
	end if
	open FS
end run

on userPicksFolder()
	set these_items to {}
	set these_items to (choose file with prompt "Select a file to convert to PDF:" of type {"JPEG", "GIFf", "PICT", "TIFF", "TEXT", "RTF"}) as list
end userPicksFolder

on open these_items
	set thesefiles to {}
	set the item_info to {}
	repeat with i from 1 to the count of these_items
		set this_item to (item i of these_items)
		set the item_info to info for this_item
		
		if folder of the item_info is true then --if the item is a folder
			processFolder(this_item)
		else if ((folder of the item_info is false) and (alias of the item_info is false)) and (the file type of the item_info is in the type_list) or ((the name extension of the item_info) is in the extension_list) then
			
			set theFilePath to (item i of these_items as string)
			set thePOSIXFilePath to POSIX path of theFilePath as string
			processFile(thePOSIXFilePath)
		end if
	end repeat
end open

--process folders 
on processFolder(theFolder)
	set these_items to list folder theFolder without invisibles
	repeat with i from 1 to the count of these_items
		set this_item to alias ((theFolder as text) & (item i of these_items))
		set the item_info to info for this_item
		if folder of the item_info is true then
			processFolder(this_item)
		else if (alias of the item_info is false) and ((the file type of the item_info is in the type_list) or the name extension of the item_info is in the extension_list) then
			set theFilePath to (this_item as string)
			set thePOSIXFilePath to POSIX path of theFilePath as string
			processFile(thePOSIXFilePath)
		end if
	end repeat
end processFolder

on processFile(thePOSIXFileName)
	try
		set terminalCommand to ""
		set convertCommand to "/System/Library/Printers/Libraries/./convert "
		set newFileName to thePOSIXFileName & ".pdf"
		set terminalCommand to convertCommand & "-f " & "\"" & thePOSIXFileName & "\"" & " -o " & "\"" & newFileName & "\"" & " -j \"application/pdf\""
		
		do shell script terminalCommand
	end try
end processFile

