default {
	state_entry() {
		llListen(PUBLIC_CHANNEL, "", llGetOwner(), "");
		llOwnerSay("Please Enter a Dimension for an Identity Matrix.");
	}
	listen(integer iChannel, string sName, key kId, string sMessage) {
		llOwnerSay("You entered "+sMessage+".");
		list lMatrix = [];
		integer x = 0;
		integer n = (integer)sMessage;
		for(x=0 ; x<n*n ; x++) {
			lMatrix += [(integer)(((x+1)%(n+1))==1)];
		}
		//llOwnerSay("["+llList2CSV(lMatrix)+"]");
		for(x=0 ; x<n ; x++) {
			llOwnerSay("["+llList2CSV(llList2ListStrided(lMatrix, x*n, (x+1)*n-1, 1))+"]");
		}
	}
}
