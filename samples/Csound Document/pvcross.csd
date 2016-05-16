<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
-odac   ;;;realtime audio out
;-iadc    ;;;uncomment -iadc if realtime audio input is needed too
; For Non-realtime ouput leave only the line below:
; -o pvcross.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>

sr = 44100 
ksmps = 32 
0dbfs  = 1 
nchnls = 2

instr 1
; analyze "beats.wav", "flute.aiff" and "mary.wav" with PVANAL first
ktime1 line 0, p3, 2			; used as index in the "beats.pvx" file
ktime2 line 0, p3, 2.6			; used as index in the "flute.pvx" or "mary.pvx"	
       pvbufread ktime1, "beats.pvx"	;take only amplitude from "beats.pvx"
if p4 = 0 then
asig   pvcross	ktime2, 1, "flute.pvx", 1, 0 ;and keep freqs of "flute.aiff"
asig   = asig*.8			;scale output
else
asig   pvcross	ktime2, 1, "mary.pvx", 1, 0 ;and keep freqs of "mary.wav"
asig   = asig*.4			;scale output
endif
       outs asig, asig

endin
</CsInstruments>
<CsScore>
i 1 0 3 0
i 1 + 3 1

e
</CsScore>
</CsoundSynthesizer>
