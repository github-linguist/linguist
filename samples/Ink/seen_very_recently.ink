/*
	Tests if the flow passes a particular gather "very recently" - that is, within the last 3 turns.

	Usage: 

	- (welcome)
		"Welcome!"
	- (opts)
		*	{seen_very_recently(->welcome)}
			"Sorry, hello, yes."
		+	"Er, what?"
			-> opts
		*	"Can we get on with it?"
		
*/

=== function seen_very_recently(-> x)
    ~ return TURNS_SINCE(x) >= 0 && TURNS_SINCE(x) <= 3