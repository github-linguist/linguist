default {
	state_entry() {
		llListen(PUBLIC_CHANNEL, "", llGetOwner(), "");
		llOwnerSay("Say a Number and I'll Increment it.");
	}
	listen(integer iChannel, string sName, key kId, string sMessage) {
		llOwnerSay("You said '"+sMessage+"' + 1 = "+(string)(((integer)sMessage)+1));
	}
}
