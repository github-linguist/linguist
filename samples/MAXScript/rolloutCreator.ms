-------------------------------------------------------------------------------
--   
--  File:		rolloutCreator.ms
--  Description:	Localization friendly helper struct for dynamically creating rollouts
--  By:		Ravi Karra [Discreet] 			ravi.karra@discreet.com
--	 
--  Version:	1.01
--  Version:	1.02 - Larry Minton [Discreet] 
--						changed <string1> += <string2> to append string1 string2
--						added addText method
--  Declarations:
/*
	rolloutCreator <rollout_name> <rollout_caption> [width:] [height:]
		creates an instance of rolloutCreator, assign it to a variable
		width - width of the rollout/dialog to be created
		height - of the rollout/dialog to be created
		
		eg:
	 	rci = rolloutCreator "myRollout" "My Rollout"
		

	.begin()
		this function needs to be called immediately after the instance is created, this does the initialization

	.addLocal <local_name> [init:]
		<local_name> 
		name of the local
		
		[init:]
		what the local should be initialized to
	
	.addControl <control_type> <control_name> <control_caption> [paramStr:<string>] =
		adds a control to the rollout
		
		<control_type>
		can be any of named rolloutControls eg: #button, #spinner, #activeXControl etc
		
		<control_name>
		variable name of the control by which it is referred eg: #btnButton
		
		<control_caption>
		caption of the control "My Button"
		
		[paramStr:]
		an optional string representation of all the keyword parameters that needs to be passed to the control
		eg: "width:100 height:20 align:#right"
		
		eg:
		rci.addControl #button #myButton "My Button"
	
	.addHandler <control_name> <event_type> [paramStr:<string>]  [codeStr:<string>] [filter:<boolean>]
		adds an event handler for the controls previously added 
		
		<control_name>
		the variable passed during the control creation
		 
		<event_type>
		any of the events supported by the control, eg: #changed, #pressed, #selected
		
		[paramStr:<string>]
		an optional string representation of all the positional and keyword parameters that are passed to the event	

		[codeStr:<string>]
		a string representation of the event handler code, if the string contains sub-strings, enclose them in two character '@' 
		and pass on\true for the filter: parameter

		[filter:<boolean>]
		if true, converts '@' to quote in codeStr
				
		eg:
		rci.addHandler #myButton #pressed codeStr:"MessageBox @Hey@" filter:on
		will add an event handler for button named "myButton". When the button is clicked, messagebox pops up with text "hey" in it.
		
	.addText <string> [filter:<boolean>]
		adds string to rollout definition. Typically used for function definitions.

		[filter:<boolean>]
		if true, converts '@' to quote in string
				
	.end()
		this function has to be called whenever all the required control and their event handler's are called. This function forms 
		the rollout string, evaluates it and returns the definition which can passed to createDialog and addRollout functions.
	
	Complete Example:
		 rci = rolloutCreator "myRollout" "My Rollout"
		 rci.begin()
			rci.addControl #button #myButton "My Button"
			rci.addHandler #myButton #pressed filter:on codeStr:"MessageBox @Isn't this cool@ title:@Wow@"
		createDialog (rci.end())
*/
-------------------------------------------------------------------------------


if __rcCounter == undefined then global __rcCounter = 0
struct rolloutCreator
(
	-- variables
	name, caption, str, def, width, height, quote="\"",
	
	-- functions
	fn begin = 
( 
		if name == undefined then
	(
			__rcCounter += 1
			name = "rolloutCreator" + __rcCounter as string
	)
		if caption == undefined then caption = ""
		str = ""
	),
	
	fn addLocal name init: = 
(
		local dStr = "\tlocal " + name as string
		if init != unsupplied then append dStr (" = " + init as string)
		append dStr "\n"
		append str dStr
	),

	fn addControl type name caption paramStr:"" =
(	
		append str ("\t" + type as string + " " + name as string + " " + quote + caption + quote + paramStr + "\n")
	),
	
	fn strFilter codeStr =
(
			local last_is_at = codeStr[codeStr.count] == "@"
			local fltStr = filterString codeStr "@"
			local rep = "\""
			codeStr = (if (codeStr[1] == "@") then rep else "") + fltStr[1]
			for i=2 to fltStr.count do
		(
				append codeStr (rep + fltStr[i])
		)
			if last_is_at then append codeStr rep
			codeStr
	),

	fn addHandler ctrl event paramStr:"" filter:on codeStr:"" =
(
		if filter do codeStr = (strFilter codeStr)
		append str ("\non " + ctrl as string + " " + event as string + " " + paramStr as string + " do \n(\n" + codeStr + ";ok\n)\n")
	),
	
	fn addText txt filter:on =
(
		if filter do txt = (strFilter txt )
		append str ("\t " + txt + "\n")
	),
	
	fn end = 
( 
		local dStr = "rollout " + name + " " + quote + caption + quote
		if width != undefined then
 			append dStr (" width:" + width as string)
		if height != undefined then
			append dStr (" height:" + height as string)
		append dStr "\n(\n" 
		append dStr str
		append dStr "\n)\n"
		str = dStr
		def = execute str
)
)

/*-- Usage
-- Create an instance of the rolloutCreator passing the name and the caption
rfTest = rolloutCreator "rfTestN" "rfTestC" --width:300 height:100
-- Start creating the rollout
rfTest.begin()
	rfTest.addControl #button #myButton "My Button"			-- add a button
--	rfTest.addHandler #myButton #pressed filter:on codeStr:"MessageBox @Hey@"
rfTest.addHandler #myButton #pressed filter:on codeStr:"MessageBox @Look to the \@Light\@ thing@" 
rfTest.end()
createDialog rfTest.def
*/