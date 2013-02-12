// SuperCollider Examples

//boot server
s.boot;

// SC as system for sound synthesis and sound processing


// patching synth moduls by writing synth defs:

(
SynthDef("mod", {
	var sig, resfreq;	
	sig = Saw.ar(100);
	resfreq = SinOsc.kr(2) * 200 + 500;
	sig = RLPF.ar(sig, resfreq, 0.1);
	sig = sig * 0.3;
	Out.ar(0, sig);
}).play;
)	

// SuperCollider: a powerful expressive DSP language:

(
30.do { arg i;  
	{ Pan2.ar( 
		SinOsc.ar(exprand(100.0, 3000.0) * LFNoise2.kr(rrand(0.1, 0.2)).range(0.95, 1.1), 0, 
			LFNoise2.kr(rrand(0.3, 0.7)).range(0,0.5) ** 4), 
		1.0.rand2) 
	}.play 
}
)

// plot envelopes
a = Env.perc(0.05, 1, 1, -4);
b = a.delay(2);
a.test.plot;
b.test.plot;

a = Env([0.5, 1, 0], [1, 1]).plot;
a.delay(1).plot;

// examples asStream function
(
{
e = Env.sine.asStream;
5.do({
    e.next.postln;
    0.25.wait;
})}.fork
)
