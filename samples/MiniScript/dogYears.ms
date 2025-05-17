// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/dogYears.ms
// Dog Years
//
// Simple demo of input, print, while, if/else, and break.
//

while true
	ageStr = input("Enter your age: ")
	age = val(ageStr)
	if age == 0 then
		print "C'mon, don't be shy..."
	else if age < 3 then
		print "So young!  Really?  I don't think so..."
	else
		print "That's " + age*7 + " in dog years."
		if age > 30 then print "Wow, you're old!"
		break  // (exits the "while" loop)
	end if
end while
