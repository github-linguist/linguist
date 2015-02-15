integer iDEPTH = 100;
integer f(integer n) {
	if(n==0) {
		return 1;
	} else {
		return n-m(f(n - 1));
	}
}
integer m(integer n) {
	if(n==0) {
		return 0;
	} else {
		return n-f(m(n - 1));
	}
}
default {
	state_entry() {
		integer x = 0;
		string s = "";
		for(x=0 ; x<iDEPTH ; x++) {
			s += (string)(f(x))+" ";
		}
		llOwnerSay(llList2CSV(llParseString2List(s, [" "], [])));
		s = "";
		for(x=0 ; x<iDEPTH ; x++) {
			s += (string)(m(x))+" ";
		}
		llOwnerSay(llList2CSV(llParseString2List(s, [" "], [])));
	}
}
