declare name "bubble";
declare description "Production of a water drop bubble sound.";
declare license "MIT";
declare copyright "(c) 2017: Yann Orlarey, GRAME";

import("stdfaust.lib");


//---------------------------`bubble`--------------------------
// bubble(f0, trig) : produces a water drop bubble sound
//
// #### Usage
//
// ```
// bubble(f0, trig) : _
// ```
//
// Where:
//
// * ` f0 `: base frequency of bubble sound
// * `trig`: trigs the bubble sound on the rising front
//
// #### Example
//
// ```
// button("drop") : bubble(600) : _
// ```
//
// #### Reference:
//
// <http://www.cs.ubc.ca/~kvdoel/publications/tap05.pdf>
//------------------------------------------------------------

bubble(f0,trig) = os.osc(f) * (exp(-damp*time) : si.smooth(0.99))
	with {
		damp = 0.043*f0 + 0.0014*f0^(3/2);
		f = f0*(1+sigma*time);
		sigma = eta * damp;
		eta = 0.075;
		time = 0 : (select2(trig>trig'):+(1)) ~ _ : ba.samp2sec;
	};

process = button("drop") : bubble(hslider("v:bubble/freq", 600, 150, 2000, 1)) <: dm.freeverb_demo;
