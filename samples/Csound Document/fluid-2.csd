<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
; Audio out No messages   Look for midifile in folder manual/examples
-odac           -d        -F midichn_advanced.mid ;;;reatime audio out
;-iadc    ;;;uncomment -iadc if realtime audio input is needed too
; For Non-realtime ouput leave only the line below:
; -o fluid-2.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32  
nchnls = 2
0dbfs  = 1

; LOAD SOUNDFONTS
gienginenum1	fluidEngine
gienginenum2	fluidEngine
isfnum1	fluidLoad "Piano Steinway Grand Model C (21,738KB).sf2", gienginenum1, 1
; Bright Steinway, program 1, channel 1
		fluidProgramSelect	gienginenum1, 1, isfnum1, 0, 1
; Concert Steinway with reverb, program 2, channel 3
		fluidProgramSelect	gienginenum1, 3, isfnum1, 0, 2
isfnum2	fluidLoad "63.3mg The Sound Site Album Bank V1.0.SF2", gienginenum2, 1
; General MIDI, program 50, channel 2
		fluidProgramSelect	gienginenum2, 2, isfnum2, 0, 50

; SEND NOTES TO STEINWAY SOUNDFONT

instr 1 ; FluidSynth Steinway Rev
  ; INITIALIZATION
	mididefault	60, p3 ; Default duration of 60 -- overridden by score.
	midinoteonkey	p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 1
  ikey       = p4
  ivelocity  = p5
	fluidNote	gienginenum1, ichannel, ikey, ivelocity
endin

instr 2 ; GM soundfont
  ; INITIALIZATION
         mididefault   60, p3 ; Default duration of 60 -- overridden by score.
         midinoteonkey p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 2
  ikey       = p4
  ivelocity  = p5
         fluidNote gienginenum2, ichannel, ikey, ivelocity
endin

instr 3 ; FluidSynth Steinway Rev
  ; INITIALIZATION
         mididefault   60, p3 ; Default duration of 60 -- overridden by score.
         midinoteonkey p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 3
  ikey       = p4
  ivelocity  = p5
         fluidNote gienginenum1, ichannel, ikey, ivelocity
endin

; COLLECT AUDIO FROM ALL SOUNDFONTS

instr 100 ; Fluidsynth output

  iamplitude1 = 7
  iamplitude2 = 7

; AUDIO
aleft1, aright1 fluidOut   gienginenum1
aleft2, aright2 fluidOut   gienginenum2
                outs       (aleft1 * iamplitude1) + (aleft2 * iamplitude2),  \
                           (aright1 * iamplitude1) + (aright2 * iamplitude2)
endin
</CsInstruments>
<CsScore>

i 1 0 3  60  100
i 2 1 3  60  100
i 3 3 3  63  100
i 100 0 10		;run for 10seconds 
e
</CsScore>
</CsoundSynthesizer>

