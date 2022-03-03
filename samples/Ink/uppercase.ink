/*
	Converts text to uppercase. Doesn't have an inky fallback.

	Usage: 

	"Give me wine. {UPPERCASE("Give me wine!")}
	
	Required C# code:

	The external binding is as follows.

		story.BindExternalFunction("UPPERCASE", (string txt) =>
	    {
	        return txt.ToUpper();
	    });
		
*/

EXTERNAL UPPERCASE(txt)
=== function UPPERCASE(txt)
    {txt}
	