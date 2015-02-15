identification division.
program-id. countdown.
environment division.
data division.
working-storage section.
01	counter 		pic 99.
	88	counter-done	value 0.
01	counter-disp	pic Z9.
procedure division.
	perform with test after varying counter from 10 by -1 until counter-done
		move counter to counter-disp
		display counter-disp
	end-perform
	stop run.
