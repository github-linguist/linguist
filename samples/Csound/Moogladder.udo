/*
Moogladder - An improved implementation of the Moog ladder filter

DESCRIPTION
This is an new digital implementation of the Moog ladder filter based on the work of Antti Huovilainen,
described in the paper \"Non-Linear Digital Implementation of the Moog Ladder Filter\" (Proceedings of DaFX04, Univ of Napoli). 
This implementation is probably a more accurate digital representation of the original analogue filter.
This is version 2 (revised 14/DEC/04), with improved amplitude/resonance scaling and frequency correction using a couple of polynomials,as suggested by Antti.

SYNTAX
ar  Moogladder  asig, kcf, kres

PERFORMANCE
asig - input signal
kcf - cutoff frequency (Hz)
kres - resonance (0 - 1).

CREDITS
Victor Lazzarini
*/

opcode Moogladder, a, akk

   setksmps 1

ipi = 4*taninv(1)
az1 init 0             /* filter delays */
az2 init 0
az3 init 0
az4 init 0
az5 init 0
ay4 init 0
amf init 0

asig,kcf,kres  xin

if kres > 1 then
kres = 1
elseif kres < 0 then
kres = 0
endif

i2v = 40000   /* twice the \'thermal voltage of a transistor\' */
 
kfc = kcf/sr  /* sr is half the actual filter sampling rate  */
kf =  kcf/(sr*2)
/* frequency & amplitude correction  */ 
kfcr = 1.8730*(kfc^3) + 0.4955*(kfc^2) - 0.6490*kfc + 0.9988
kacr = -3.9364*(kfc^2) + 1.8409*kfc + 0.9968;
k2vg = i2v*(1-exp(-2*ipi*kfcr*kf)) /* filter tuning  */

/* cascade of 4 1st order sections         */
ay1 = az1 + k2vg*(tanh((asig - 4*kres*amf*kacr)/i2v) - tanh(az1/i2v))
az1 = ay1
ay2 = az2 + k2vg*(tanh(ay1/i2v) - tanh(az2/i2v ))
az2 = ay2
ay3 = az3 + k2vg*(tanh(ay2/i2v) - tanh(az3/i2v))
az3 = ay3
ay4 = az4 + k2vg*(tanh(ay3/i2v) - tanh(az4/i2v))
az4 = ay4
/* 1/2-sample delay for phase compensation  */
amf = (ay4+az5)*0.5
az5 = ay4

/* oversampling  */
ay1 = az1 + k2vg*(tanh((asig - 4*kres*amf*kacr)/i2v) - tanh(az1/i2v))
az1 = ay1
ay2 = az2 + k2vg*(tanh(ay1/i2v) - tanh(az2/i2v ))
az2 = ay2
ay3 = az3 + k2vg*(tanh(ay2/i2v) - tanh(az3/i2v))
az3 = ay3
ay4 = az4 + k2vg*(tanh(ay3/i2v) - tanh(az4/i2v))
az4 = ay4
amf = (ay4+az5)*0.5
az5 = ay4
        
        xout  amf
	
endop
 
