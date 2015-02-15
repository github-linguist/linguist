string sURL = "http://www.RosettaCode.Org";
key kHttpRequestId;
default {
	state_entry() {
		kHttpRequestId = llHTTPRequest(sURL, [], "");
	}
	http_response(key kRequestId, integer iStatus, list lMetaData, string sBody) {
		if(kRequestId==kHttpRequestId) {
			llOwnerSay("Status="+(string)iStatus);
			integer x = 0;
			for(x=0 ; x<llGetListLength(lMetaData) ; x++) {
				llOwnerSay("llList2String(lMetaData, "+(string)x+")="+llList2String(lMetaData, x));
			}
			list lBody = llParseString2List(sBody, ["\n"], []);
			for(x=0 ; x<llGetListLength(lBody) ; x++) {
				llOwnerSay("llList2String(lBody, "+(string)x+")="+llList2String(lBody, x));
			}
		}
	}
}
