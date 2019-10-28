// WARNING: This a "legacy example based on a deprecated library". Check filters.lib
// for more accurate examples of filter functions

declare name 		"bandFilter";
declare version 	"1.0";
declare author 		"Grame";
declare license 	"BSD";
declare copyright 	"(c)GRAME 2006";

import("stdfaust.lib");

//---------------------second order filter--------------------------
// filter(Q,F,G)
//  			Q : quality factor [1..100]
//				F :	frequency (Hz)
//				G : gain [0..1]
//------------------------------------------------------------------

filter(Q,F,G)	= fi.TF2(  (1 +  K/Q + K*K) 	/ D,
						 2 * (K*K - 1) 		/ D,
						(1 - K/Q + K*K) 	/ D,
						 2 * (K*K - 1) 		/ D,
						(1 - V*K/Q + K*K) 	/ D
					 )
		with {
				V = ba.db2linear(G);
				K = tan(ma.PI*F/ma.SR);
				D = 1 + V*K/Q + K*K;
		};

//--------------- Band Filter with user interface ------------------
// bandfilter(F)
//  			F :	default frequency (Hz)
//
//------------------------------------------------------------------

bandfilter(F)	= filter(	nentry("Q factor [style:knob]",50,0.1,100,0.1),
							nentry("freq [unit:Hz][style:knob]", F, 20, 20000, 1),
							0 - vslider("gain [unit:dB]", 0, -50, 50, 0.1)
						);

//------------------------- Process --------------------------------

process 		= vgroup("Bandfilter", bandfilter(1000));
