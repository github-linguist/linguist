// WARNING: This a "legacy example based on a deprecated library". Check filters.lib
// for more accurate examples of filter functions

declare name 		"lowcut";
declare version 	"1.0";
declare author 		"Grame";
declare license 	"BSD";
declare copyright 	"(c)GRAME 2006";

//------------------------------------------------------------------
//	DAFX, Digital Audio Effects (Wiley ed.)
//	chapter 2 	: filters
//	section 2.3 : Equalizers
//	page 53 	: second order shelving filter design
//------------------------------------------------------------------

import("stdfaust.lib");

//------------------- low-frequency shelving cut (table 2.3) --------------------

V0(g)			= pow(10,g/-20.0);
K(fc) 			= tan(ma.PI*fc/ma.SR);
squ(x)		= x*x;
denom(fc,g)		= 1 + sqrt(2*V0(g))*K(fc) + V0(g)*squ(K(fc));

lfcut(fc, g)	= fi.TF2((1 + sqrt(2)*K(fc) + squ(K(fc))) / denom(fc,g),
						2 * (squ(K(fc)) - 1) / denom(fc,g),
						(1 - sqrt(2)*K(fc) + squ(K(fc))) / denom(fc,g),
						2 * (V0(g)*squ(K(fc)) - 1) / denom(fc,g),
						(1 - sqrt(2*V0(g))*K(fc) + V0(g)*squ(K(fc))) / denom(fc,g));

//------------------------------ User Interface -----------------------------------

freq 			= hslider("freq [unit:Hz][style:knob]", 100, 20, 5000, 1);
att				= hslider("attenuation [unit:dB][style:knob]", 0, -96, 10, 0.1);

//----------------------------------- Process -------------------------------------

process 		= vgroup("low-freq shelving cut", lfcut(freq,att));

