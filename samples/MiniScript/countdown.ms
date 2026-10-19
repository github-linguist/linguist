// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/countdown.ms
// Countdown
// This is the tiniest Mini Micro demo.
// It shows basic looping, plus use of range, print, and wait.

for i in range(10,1)
	print i + "..."
	wait
end for
print "BLASTOFF!"
