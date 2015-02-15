default {
	state_entry() {
		llOwnerSay("llGetTimestamp()="+(string)llGetTimestamp());
		llOwnerSay("llGetEnergy()="+(string)llGetEnergy());
		llOwnerSay("llGetFreeMemory()="+(string)llGetFreeMemory());
		llOwnerSay("llGetMemoryLimit()="+(string)llGetMemoryLimit());
	}
}
