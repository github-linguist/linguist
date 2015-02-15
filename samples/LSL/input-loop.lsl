string sNOTECARD = "Input_Loop_Data_Source.txt";
default {
	integer iNotecardLine = 0;
	state_entry() {
		llOwnerSay("Reading '"+sNOTECARD+"'");
		llGetNotecardLine(sNOTECARD, iNotecardLine);
	}
	dataserver(key kRequestId, string sData) {
		if(sData==EOF) {
			llOwnerSay("EOF");
		} else {
			llOwnerSay((string)iNotecardLine+": "+sData);
			llGetNotecardLine(sNOTECARD, ++iNotecardLine);
		}
	}
}
