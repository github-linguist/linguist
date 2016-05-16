<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
-odac     ;;;RT audio out
;-iadc    ;;;uncomment -iadc if RT audio input is needed too
; For Non-realtime ouput leave only the line below:
; -o adsr.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1

instr 1

iatt  = p5
idec  = p6  
islev = p7
irel  = p8

kenv	adsr iatt, idec, islev, irel
kcps =  cpspch(p4) 	  ;frequency

asig	vco2  kenv * 0.8, kcps
	outs  asig, asig

endin

</CsInstruments>
<CsScore>

i 1  0  2  7.00  .0001  1  .5  .001 ; short attack
i 1  3  2  7.02  1  .5  .5  .001    ; long attack
i 1  6  2  6.09  .0001  1 .5  .7     ; long release

e

</CsScore>
</CsoundSynthesizer>
