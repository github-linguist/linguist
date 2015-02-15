integer Fibonacci(integer n) {
	if(n<2) {
		return n;
	} else {
		return Fibonacci(n-1)+Fibonacci(n-2);
	}
}
default {
	state_entry() {
		integer x = 0;
		for(x=0 ; x<35 ; x++) {
			llOwnerSay("Fibonacci("+(string)x+")="+(string)Fibonacci(x));
		}
	}
}
