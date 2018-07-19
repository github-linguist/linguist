integer iLimit_of_Recursion = 0;
Find_Limit_of_Recursion(integer x) {
	llOwnerSay("x="+(string)x);
	iLimit_of_Recursion = x;
	Find_Limit_of_Recursion(x+1);
}
default {
	state_entry() {
		Find_Limit_of_Recursion(0);
		llOwnerSay("iLimit_of_Recursion="+(string)iLimit_of_Recursion);
	}
}
