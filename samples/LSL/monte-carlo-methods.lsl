integer iMIN_SAMPLE_POWER = 0;
integer iMAX_SAMPLE_POWER = 6;
default {
	state_entry() {
		llOwnerSay("Estimating Pi ("+(string)PI+")");
		integer iSample = 0;
		for(iSample=iMIN_SAMPLE_POWER ; iSample<=iMAX_SAMPLE_POWER  ; iSample++) {
			integer iInCircle = 0;
			integer x = 0;
			integer iMaxSamples = (integer)llPow(10, iSample);
			for(x=0 ; x<iMaxSamples ; x++) {
				if(llSqrt(llPow(llFrand(2.0)-1.0, 2.0)+llPow(llFrand(2.0)-1.0, 2.0))<1.0) {
					iInCircle++;
				}
			}
			float fPi = ((4.0*iInCircle)/llPow(10, iSample));
			float fError = llFabs(100.0*(PI-fPi)/PI);
			llOwnerSay((string)iSample+": "+(string)iMaxSamples+" = "+(string)fPi+", Error = "+(string)fError+"%");
		}
		llOwnerSay("Done.");
	}
}
